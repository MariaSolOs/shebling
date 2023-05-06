use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Error, Fields,
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
            let mut new_attrs = field
                .attrs
                .into_iter()
                .filter(|attr| attr.path().is_ident("new"));

            // Check that there is at most one #[new] attribute.
            let new_attr = new_attrs.next();
            if let Some(attr) = new_attrs.next() {
                return error(attr.span(), "duplicate #[new(...)] attribute");
            }

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

/// Creates a `From<..>` implementation for each struct variant of the attached
/// enum.
///
/// # Example
/// ```
/// use shebling_codegen::from_structs;
///
/// struct Foo {
///     foo: u32,
/// }
///
/// #[from_structs]
/// enum Bar {
///     Foo(Foo),
/// }
///
/// let bar = Bar::from(Foo { foo: 17 });
/// assert!(matches!(bar, Bar::Foo(_)));
/// ```
#[proc_macro_attribute]
pub fn from_structs(_args: TokenStream, input: TokenStream) -> TokenStream {
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
        if let Fields::Unnamed(FieldsUnnamed { unnamed, .. }) = &variant.fields {
            if unnamed.len() != 1 {
                return error(unnamed.span(), "expected a single struct identifier");
            }

            let struct_ty = &unnamed[0].ty;
            let variant_ident = &variant.ident;
            from_impls.push(quote! {
                impl #impl_generics From<#struct_ty> for #ident #ty_generics #where_clause {
                    fn from(inner: #struct_ty) -> Self {
                        #ident::#variant_ident(inner)
                    }
                }
            });
        }
    }

    let output = quote! {
        #input
        #(#from_impls)*
    };

    output.into()
}

fn error(span: Span, message: &str) -> TokenStream {
    Error::new(span, message).to_compile_error().into()
}
