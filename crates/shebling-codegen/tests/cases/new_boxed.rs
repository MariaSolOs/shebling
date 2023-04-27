use shebling_codegen::New;

#[derive(New)]
struct Foo {
    foo: Box<String>,
}

#[derive(New)]
struct Bar {
    #[new(into)]
    bar: Box<String>,
}

fn main() {
    // In both cases, new() will box the parameter.
    let foo = Foo::new("foo".into());
    assert_eq!(*foo.foo, "foo");

    let bar = Bar::new("bar");
    assert_eq!(*bar.bar, "bar");
}
