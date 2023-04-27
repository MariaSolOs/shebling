use shebling_codegen::New;

// Invalid uses of the #[new] attribute.

#[derive(New)]
struct Foo {
    #[new]
    foo: String,
}

#[derive(New)]
struct Bar {
    #[new()]
    bar: String,
}

#[derive(New)]
struct Baz {
    #[new(wrong)]
    baz: String,
}

fn main() {}
