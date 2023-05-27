use super::*;

#[test]
fn lit_with_escapes() {
    insta::assert_debug_snapshot!(tokenize_source("foo\\ bar"), @r###"
    (
        [
            Spanned(
                Word(
                    [
                        Spanned(
                            Lit(
                                "foo bar",
                            ),
                            Span {
                                start: 0,
                                end: 8,
                            },
                        ),
                    ],
                ),
                Span {
                    start: 0,
                    end: 8,
                },
            ),
        ],
        [],
    )
    "###);
    insta::assert_debug_snapshot!(tokenize_source("\\$0"), @r###"
    (
        [
            Spanned(
                Word(
                    [
                        Spanned(
                            Lit(
                                "$0",
                            ),
                            Span {
                                start: 0,
                                end: 3,
                            },
                        ),
                    ],
                ),
                Span {
                    start: 0,
                    end: 3,
                },
            ),
        ],
        [],
    )
    "###);
    insta::assert_debug_snapshot!(tokenize_source("foo\\\nbar"), @r###"
    (
        [
            Spanned(
                Word(
                    [
                        Spanned(
                            Lit(
                                "foobar",
                            ),
                            Span {
                                start: 0,
                                end: 8,
                            },
                        ),
                    ],
                ),
                Span {
                    start: 0,
                    end: 8,
                },
            ),
        ],
        [],
    )
    "###);
}

#[test]
fn operator_prefixes() {
    // Make sure that we correctly handle some operators being prefixes of others.
    insta::assert_debug_snapshot!(tokenize_source(">>&"), @r###"
    (
        [
            Spanned(
                RedirOp(
                    DGreat,
                ),
                Span {
                    start: 0,
                    end: 2,
                },
            ),
            Spanned(
                ControlOp(
                    And,
                ),
                Span {
                    start: 2,
                    end: 3,
                },
            ),
        ],
        [],
    )
    "###);
    insta::assert_debug_snapshot!(tokenize_source("|||||"), @r###"
    (
        [
            Spanned(
                ControlOp(
                    OrIf,
                ),
                Span {
                    start: 0,
                    end: 2,
                },
            ),
            Spanned(
                ControlOp(
                    OrIf,
                ),
                Span {
                    start: 2,
                    end: 4,
                },
            ),
            Spanned(
                ControlOp(
                    Or,
                ),
                Span {
                    start: 4,
                    end: 5,
                },
            ),
        ],
        [],
    )
    "###);
}
