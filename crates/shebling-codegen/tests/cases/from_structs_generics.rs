use shebling_codegen::FromStructs;

struct Bar<T> {
    bar: T,
}

// The attribute should handle generics just fine.
#[derive(FromStructs)]
enum Foo<T> {
    Bar(Bar<T>),
}

fn main() {
    let bar = Bar { bar: 17 };
    let foo = Foo::from(bar);

    assert!(matches!(foo, Foo::Bar(_)));
}
