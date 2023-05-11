use trybuild::TestCases;

#[test]
fn test() {
    let t = TestCases::new();

    // #[derive(New)] tests
    t.pass("tests/cases/new_boxed.rs");
    t.pass("tests/cases/new_default.rs");
    t.pass("tests/cases/new_into.rs");
    t.compile_fail("tests/cases/new_invalid_attr.rs");
    t.pass("tests/cases/new_param_ty.rs");

    // #[derive(FromStructs)] tests
    t.pass("tests/cases/from_structs_generics.rs");
    t.pass("tests/cases/from_structs_ignore_non_structs.rs");
    t.pass("tests/cases/from_structs_ignore_variant.rs");
}
