use super::*;

pub(super) fn identifier(span: ParseSpan) -> ParseResult<String> {
    recognize_string(pair(
        // Make sure that the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(span)
}

fn word_sgmt(span: ParseSpan) -> ParseResult<WordSgmt> {
    alt((
        map(spanned(single_quoted), WordSgmt::SingleQuoted),
        map(double_quoted, WordSgmt::DoubleQuoted),
    ))(span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    use insta::assert_debug_snapshot;

    #[test]
    fn test_identifier() {
        // Valid identifiers.
        assert_debug_snapshot!(tests::parse_ok(identifier, "foo", ""), @r###"
        (
            [],
            "foo",
        )
        "###);
        assert_debug_snapshot!(tests::parse_ok(identifier, "_foo0", ""), @r###"
        (
            [],
            "_foo0",
        )
        "###);

        // Identifiers cannot start with a number.
        assert_debug_snapshot!(tests::parse_fail(identifier, "0foo"), @r###"
        (
            ParseError {
                location: 0,
                notes: [],
            },
            [],
        )
        "###);
    }
}
