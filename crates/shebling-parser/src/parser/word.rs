use super::*;

pub(super) fn identifier(span: ParseSpan) -> ParseResult<String> {
    recognize_string(pair(
        // Make sure the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
        // TODO: map(
        //     alt((extglob, recognize_string(one_of("*?")), bracketed_glob)),
        //     WordSgmt::Glob,
        //     ),
        //     // Fallback for other glob prefix characters:
        // TODO: map(one_of("@!+["), |c| WordSgmt::Lit(c.into())),
        // TODO: dollar_sgmt,
        // TODO: map(brace_expansion, WordSgmt::BraceExpansion),
        // TODO: map(backquoted, WordSgmt::BackQuoted),
        // TODO: map(proc_sub, WordSgmt::ProcSub),
        // TODO: lit_word_sgmt(pattern),
        //     // Literal curly braces:
        // TODO: map(alt((recognize_string(tag("{}")), lit_curly)), WordSgmt::Lit),
    ))(span)
}

fn word_sgmt(span: ParseSpan) -> ParseResult<WordSgmt> {
    alt((
        map(spanned(single_quoted), WordSgmt::SingleQuoted),
        into(double_quoted),
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
