use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Attribute, Error, Fields,
    FieldsNamed, FieldsUnnamed, GenericArgument, ItemEnum, ItemStruct, PathArguments, Type,
};

/// Creates an implementation for a `new()` function that initializes
/// the attached struct.
///
/// # Example
/// ```
/// use shebling_codegen::New;
///
/// #[derive(New)]
/// struct Foo {
///    // Any type that implements Into<String> can be used as the
///    // first parameter for Foo::new.
///    #[new(into)]
///    foo: String,
///    // With boxed types, the generated constructor will take
///    // the unboxed type as a parameter.
///    bar: Box<u32>,
/// }
///
/// let foo = Foo::new("Foo", 10);
/// assert_eq!((foo.foo, *foo.bar), ("Foo".into(), 10));
/// ```
#[proc_macro_derive(New, attributes(new))]
pub fn new_derive(input: TokenStream) -> TokenStream {
    let ItemStruct {
        ident,
        fields,
        generics,
        ..
    } = parse_macro_input!(input as ItemStruct);

    if let Fields::Named(FieldsNamed { named, .. }) = fields {
        let mut param_list = Vec::with_capacity(named.len());
        let mut assigns = Vec::with_capacity(named.len());

        for field in named {
            // Get the #[new] attribute if present.
            let new_attr = match helper_attr("new", &field.attrs) {
                Ok(attr) => attr,
                Err(err) => return err,
            };

            // Check if we should use Into<..> or the type's default for the
            // constructor parameter.
            let (mut use_into, mut use_default) = (false, false);
            if let Some(new_attr) = new_attr {
                if let Err(err) = new_attr.parse_nested_meta(|meta| match meta.path.get_ident() {
                    Some(ident) if ident == "into" => {
                        use_into = true;

                        Ok(())
                    }
                    Some(ident) if ident == "default" => {
                        use_default = true;

                        Ok(())
                    }
                    _ => Err(meta.error("invalid #[new(...)] content")),
                }) {
                    return err.into_compile_error().into();
                }
            }

            // Get the type to use in the parameter list and the field assignment value.
            let field_ident = field.ident.expect("Field should be named.");
            let field_ty = field.ty;
            let (mut param_ty, mut field_assign) = if use_into {
                (
                    Some(quote!(impl Into<#field_ty>)),
                    quote!(#field_ident.into()),
                )
            } else if use_default {
                (None, quote!(Default::default()))
            } else {
                (Some(quote!(#field_ty)), quote!(#field_ident))
            };

            match field_ty {
                Type::Path(ty) => {
                    let sgmt = if ty.path.segments.len() != 1 {
                        return error(ty.span(), "expected a single-segmented type path");
                    } else {
                        &ty.path.segments[0]
                    };

                    // If the type is boxed, use the inner type as the parameter type
                    // and box the parameter when assigning the field.
                    if !use_default && sgmt.ident == "Box" {
                        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            args,
                            ..
                        }) = &sgmt.arguments
                        {
                            if args.len() != 1 {
                                return error(
                                    ty.span(),
                                    "Box<..> should have a single generic argument",
                                );
                            }

                            let inner_ty = if let GenericArgument::Type(ty) = &args[0] {
                                ty
                            } else {
                                return error(ty.span(), "expected a generic type");
                            };
                            param_ty = Some(if use_into {
                                quote!(impl Into<#inner_ty>)
                            } else {
                                quote!(#inner_ty)
                            });

                            field_assign = quote!(Box::new(#field_assign));
                        } else {
                            return error(sgmt.span(), "invalid Box<..> type");
                        }
                    }
                }
                Type::Tuple(_) => {
                    if use_into {
                        return error(field_ty.span(), "can't use #[new(...)] with tuple types");
                    }
                }
                _ => return error(field_ty.span(), "unexpected field type"),
            }

            assigns.push(quote!(#field_ident: #field_assign));
            if let Some(param_ty) = param_ty {
                param_list.push(quote!(#field_ident: #param_ty));
            }
        }

        // Write the new() implementation.
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let new_impl = quote! {
            impl #impl_generics #ident #ty_generics #where_clause {
                pub(crate) fn new(#(#param_list),*) -> Self {
                    Self { #(#assigns),* }
                }
            }
        };

        new_impl.into()
    } else {
        error(
            fields.span(),
            "only structs with named fields are supported.",
        )
    }
}

/// Creates a `From<..>` implementation for selected variants of the attached
/// enum.
///
/// # Example
/// ```
/// use shebling_codegen::FromVariants;
///
/// struct Bar {
///     bar: u32,
/// }
///
/// #[derive(FromVariants)]
/// enum Foo {
///     Bar(Bar),
/// }
///
/// let foo = Foo::from(Bar { bar: 17 });
/// assert!(matches!(foo, Foo::Bar(_)));
/// ```
#[proc_macro_derive(FromVariants, attributes(from))]
// TODO: Add an option for just considering some variants.
pub fn from_variants_derive(input: TokenStream) -> TokenStream {
    // Parse the input enum.
    let input @ ItemEnum {
        ident,
        variants,
        generics,
        ..
    } = &parse_macro_input!(input as ItemEnum);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    if variants.is_empty() {
        return error(input.span(), "unexpected empty enum");
    }

    // For each enum variant with an inner struct, generate the From<..> implementation.
    let mut from_impls = Vec::with_capacity(variants.len());
    for variant in variants {
        // Ignore the arm if it has a #[from(ignore)] attribute.
        if let Some(attr) = match helper_attr("from", &variant.attrs) {
            Ok(attr) => attr,
            Err(err) => return err,
        } {
            if let Err(err) = attr.parse_nested_meta(|meta| match meta.path.get_ident() {
                Some(ident) if ident == "ignore" => Ok(()),
                _ => Err(meta.error("invalid #[from(...)] content")),
            }) {
                return err.into_compile_error().into();
            } else {
                continue;
            }
        }

        if let Fields::Unnamed(FieldsUnnamed { unnamed, .. }) = &variant.fields {
            if unnamed.len() != 1 {
                return error(unnamed.span(), "expected a single struct identifier");
            }

            let struct_ty = &unnamed[0].ty;
            let variant_ident = &variant.ident;
            from_impls.push(quote! {
                impl #impl_generics ::std::convert::From<#struct_ty> for #ident #ty_generics #where_clause {
                    fn from(inner: #struct_ty) -> Self {
                        #ident::#variant_ident(inner)
                    }
                }
            });
        }
    }

    let output = quote!(#(#from_impls)*);
    output.into()
}

fn helper_attr<'a>(
    ident: &str,
    attrs: &'a [Attribute],
) -> Result<Option<&'a Attribute>, TokenStream> {
    // Get the helper attributes.
    let mut attrs = attrs.iter().filter(|attr| attr.path().is_ident(ident));

    // Check that there is at most one.
    let attr = attrs.next();
    if let Some(attr) = attrs.next() {
        return Err(error(
            attr.span(),
            format!("duplicate #[{}(...)] attribute", ident),
        ));
    } else {
        Ok(attr)
    }
}

fn error(span: Span, message: impl AsRef<str>) -> TokenStream {
    Error::new(span, message.as_ref()).to_compile_error().into()
}
