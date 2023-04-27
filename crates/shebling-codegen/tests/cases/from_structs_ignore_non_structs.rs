use shebling_codegen::from_structs;

// Make sure that variants that are not structs are ignored
// without errors.

struct Foo {
    foo: bool,
}

#[from_structs]
enum Bar {
    Foo(Foo),
    Baz,
    Qux { qux: u32 },
}

fn main() {
    let foo = Foo { foo: true };
    let bar: Bar = foo.into();
    let _other_bar = Bar::Qux { qux: 17 };

    assert!(matches!(bar, Bar::Foo(_)));
}
