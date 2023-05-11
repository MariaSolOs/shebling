use shebling_codegen::FromVariants;

// Make sure that variants that are not structs are ignored
// without errors.

struct Bar {
    bar: bool,
}

#[derive(FromVariants)]
enum Foo {
    Bar(Bar),
    Baz,
    Qux { qux: u32 },
}

fn main() {
    let bar = Bar { bar: true };
    let foo: Foo = bar.into();
    let _other_foo = Foo::Qux { qux: 17 };

    assert!(matches!(foo, Foo::Bar(_)));
}
