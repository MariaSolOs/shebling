use super::*;

pub(crate) fn single_quoted(span: ParseSpan) -> ParseResult<String> {
    // Parse the opening quote and the string.
    let (span, string) = preceded(char('\''), recognize_string(take_till(|c| c == '\'')))(span)?;

    // Check that the ending quote isn't escaped.
    let last_char = string.chars().last().unwrap_or_default();
    if last_char == '\\' {
        span.diag(
            Diagnostic::builder(DiagnosticKind::BadEscape)
                .label("the backslash before this quote is literal", span.offset())
                .help("Wanna escape a single quote? 'Let'\\''s do it correctly'"),
        );
    }

    // Verify the closing quote.
    let (span, quote) = spanned(cut(context("expected ending single quote!", char('\''))))(span)?;

    // Apostrophe check.
    let (span, alphabetic_follows) = peeked(satisfy(|c| c.is_ascii_alphabetic()))(span)?;
    if alphabetic_follows && last_char.is_ascii_alphabetic() {
        span.diag(
            Diagnostic::builder(DiagnosticKind::BadQuote)
                .label("this apostrophe terminates the string!", quote)
                .help("Try escaping the apostrophe, 'it'\\''s done like this!'"),
        );
    }

    Ok((span, string))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    use insta::assert_debug_snapshot;

    #[test]
    fn test_single_quoted() {
        // A valid single quoted string.
        assert_debug_snapshot!(tests::parse_ok(single_quoted, "'foo bar'", ""), @r###"
        (
            [],
            "foo bar",
        )
        "###);

        // Suspicious closing quotes.
        assert_debug_snapshot!(tests::parse_ok(single_quoted, "'let's", "s"), @r###"
        (
            [
                (
                    "shebling::bad_quote",
                    SourceSpan {
                        offset: SourceOffset(
                            4,
                        ),
                        length: 1,
                    },
                ),
            ],
            "let",
        )
        "###);
        assert_debug_snapshot!(tests::parse_ok(single_quoted, "'let\\'s", "s"), @r###"
        (
            [
                (
                    "shebling::bad_escape",
                    SourceSpan {
                        offset: SourceOffset(
                            5,
                        ),
                        length: 0,
                    },
                ),
            ],
            "let\\",
        )
        "###);

        // Unclosed string.
        assert_debug_snapshot!(tests::parse_fail(single_quoted, "'foo"), @r###"
        (
            ParseError {
                location: 4,
                notes: [
                    ParseErrorNote {
                        location: 4,
                        note: "expected ending single quote!",
                    },
                ],
            },
            [],
        )
        "###);
    }
}
