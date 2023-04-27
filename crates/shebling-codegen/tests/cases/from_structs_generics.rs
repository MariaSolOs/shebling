use shebling_codegen::from_structs;

// The attribute should handle generics just fine.

struct Foo<T> {
    foo: T,
}

#[from_structs]
enum Bar<T> {
    Foo(Foo<T>),
}

fn main() {
    let foo = Foo { foo: 17 };
    let bar = Bar::from(foo);

    assert!(matches!(bar, Bar::Foo(_)));
}
