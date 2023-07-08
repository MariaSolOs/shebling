use super::*;

const DOUBLE_ESCAPABLE: &str = "\\\"$`";

fn backslash(span: ParseSpan) -> ParseResult<char> {
    char('\\')(span)
}

pub(super) fn double_quoted(span: ParseSpan) -> ParseResult<DoubleQuoted> {
    fn lit(span: ParseSpan) -> ParseResult<String> {
        alt((
            map(
                many1(alt((
                    escaped(DOUBLE_ESCAPABLE),
                    recognize_string(is_not(DOUBLE_ESCAPABLE)),
                ))),
                |sgmts| sgmts.concat(),
            ),
            recognize_string(char('$')),
        ))(span)
    }

    delimited(
        char('"'),
        map(
            many0(alt((
                // TODO: dollar_exp
                into(spanned(lit)),
                // TODO: escaping_backquoted
            ))),
            DoubleQuoted::new,
        ),
        context("expected ending double quote!", char('"')),
    )(span)
}

fn escaped<'a>(can_escape: &'static str) -> impl FnMut(ParseSpan<'a>) -> ParseResult<String> {
    alt((
        map(line_continuation, |_| String::new()),
        map(pair(backslash, anychar), move |(bs, c)| {
            if can_escape.contains(c) {
                c.into()
            } else {
                format!("{}{}", bs, c)
            }
        }),
    ))
}

pub(super) fn line_continuation(span: ParseSpan) -> ParseResult<()> {
    swallow(pair(backslash, newline))(span)
}

pub(super) fn single_quoted(span: ParseSpan) -> ParseResult<String> {
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
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::parser::tests;

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

    #[test]
    fn test_double_quoted() {
        // TODO:
    }
}
