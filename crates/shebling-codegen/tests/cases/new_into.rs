use shebling_codegen::New;

#[derive(New)]
struct Foo {
    foo: String,
}

#[derive(New)]
struct Bar {
    #[new(into)]
    bar: String,
}

fn main() {
    // Without #[new(into)], the parameter must be of type String.
    let foo = Foo::new("foo".into());
    assert_eq!(foo.foo, "foo");

    // With the attribute, any string-like thing works.
    let bar = Bar::new("bar");
    assert_eq!(bar.bar, "bar");
}
