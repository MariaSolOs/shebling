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

    // #[derive(FromVariants)] tests
    t.pass("tests/cases/from_variants_generics.rs");
    t.pass("tests/cases/from_variants_ignore.rs");
    t.pass("tests/cases/from_variants_non_structs.rs");
}
