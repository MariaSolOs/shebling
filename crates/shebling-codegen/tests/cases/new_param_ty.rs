use shebling_codegen::New;

#[derive(New)]
struct Foo {
    bar: String,
    baz: (u32, u32),
}

fn main() {
    // Parameter types can be paths or tuples.
    let foo = Foo::new("foo".into(), (1, 2));
    assert_eq!(foo.bar, "foo");
    assert_eq!(foo.baz.0, 1);
}
