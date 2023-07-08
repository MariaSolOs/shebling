use super::*;

const UNISPACES: &str = "\u{A0}\u{200B}";

fn carriage_return(span: ParseSpan) -> ParseResult<char> {
    let (span, cr) = spanned(char('\r'))(span)?;
    span.diag(Diagnostic::builder(DiagnosticKind::CarrigeReturn).span(cr));

    Ok((span, '\r'))
}

fn line_ending(span: ParseSpan) -> ParseResult<char> {
    preceded(opt(carriage_return), newline)(span)
}

fn line_space(span: ParseSpan) -> ParseResult<char> {
    alt((one_of(" \t"), |span| {
        let (span, unispace) = spanned(one_of(UNISPACES))(span)?;
        span.diag(Diagnostic::builder(DiagnosticKind::Unichar).label("unicode space", unispace));

        Ok((span, ' '))
    }))(span)
}

pub(super) fn whitespace(span: ParseSpan) -> ParseResult<char> {
    alt((line_space, carriage_return, line_ending))(span)
}

#[cfg(test)]
mod test {
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_carriage_return() {
        // Check parsed CR and warning.
        assert_debug_snapshot!(tests::parse_ok(carriage_return, "\r", ""), @r###"
        (
            [
                (
                    "shebling::carriage_return",
                    SourceSpan {
                        offset: SourceOffset(
                            0,
                        ),
                        length: 1,
                    },
                ),
            ],
            '\r',
        )
        "###);
    }

    #[test]
    fn test_line_ending() {
        // A new line is fine.
        assert_debug_snapshot!(tests::parse_ok(line_ending, "\n", ""), @r###"
        (
            [],
            '\n',
        )
        "###);

        // Parse CRLF but warn about the carriage return.
        assert_debug_snapshot!(tests::parse_ok(line_ending, "\r\n", ""), @r###"
        (
            [
                (
                    "shebling::carriage_return",
                    SourceSpan {
                        offset: SourceOffset(
                            0,
                        ),
                        length: 1,
                    },
                ),
            ],
            '\n',
        )
        "###);

        // Missing the new line.
        assert_debug_snapshot!(tests::parse_ok(line_ending, "\r", ""), @r###""###);
    }

    #[test]
    fn test_line_space() {
        // Spaces and tabs are fine.
        assert_debug_snapshot!(tests::parse_ok(line_space, " ", ""), @r###"
        (
            [],
            ' ',
        )
        "###);
        assert_debug_snapshot!(tests::parse_ok(line_space, "\t", ""), @r###"
        (
            [],
            '\t',
        )
        "###);

        // Unicode spaces are not.
        assert_debug_snapshot!(tests::parse_ok(line_space, "\u{A0}", ""), @r###"
        (
            [
                (
                    "shebling::unichar",
                    SourceSpan {
                        offset: SourceOffset(
                            0,
                        ),
                        length: 2,
                    },
                ),
            ],
            ' ',
        )
        "###);
    }
}
