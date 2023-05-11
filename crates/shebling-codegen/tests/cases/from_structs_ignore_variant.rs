use shebling_codegen::FromStructs;

// Ignore variants if they have a #[from_structs(ignore)] attribute.
#[derive(FromStructs)]
enum Foo {
    Bar(String),
    #[from_structs(ignore)]
    Baz(String),
}

fn main() {
    let _foo: Foo = String::from("foo").into();
}
