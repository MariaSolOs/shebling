use shebling_codegen::FromVariants;

// Ignore variants if they have a #[from_structs(ignore)] attribute.
#[derive(FromVariants)]
enum Foo {
    Bar(String),
    #[from(ignore)]
    Baz(String),
}

fn main() {
    let _foo: Foo = String::from("foo").into();
}
