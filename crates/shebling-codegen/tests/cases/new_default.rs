use shebling_codegen::New;
use std::cell::RefCell;

#[derive(New)]
struct Foo {
    #[new(default)]
    bar: String,
    #[new(default)]
    baz: RefCell<Vec<String>>,
}

fn main() {
    // All fields in foo should be initialized to their default values.
    let foo = Foo::new();
    assert_eq!(foo.bar, "");
    assert_eq!(foo.baz, RefCell::new(Vec::new()));
}
