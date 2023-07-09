use super::*;

pub(super) const UNISPACES: &str = "\u{A0}\u{200B}";

fn carriage_return(span: ParseSpan) -> ParseResult<char> {
    let (span, cr) = spanned(char('\r'))(span)?;
    span.diag(Diagnostic::builder(DiagnosticKind::CarrigeReturn).span(cr));

    Ok((span, '\r'))
}

fn comment(span: ParseSpan) -> ParseResult<String> {
    recognize_string(pair(char('#'), is_not("\r\n")))(span)
}

fn line_ending(span: ParseSpan) -> ParseResult<char> {
    preceded(opt(carriage_return), newline)(span)
}

pub(super) fn line_space(span: ParseSpan) -> ParseResult<char> {
    alt((one_of(" \t"), |span| {
        let (span, unispace) = spanned(one_of(UNISPACES))(span)?;
        span.diag(Diagnostic::builder(DiagnosticKind::Unichar).label("unicode space", unispace));

        Ok((span, ' '))
    }))(span)
}

pub(super) fn trivia(span: ParseSpan) -> ParseResult<String> {
    fn continued(span: ParseSpan) -> ParseResult<Vec<char>> {
        let (span, (mut continued, comment)) = preceded(
            line_continuation,
            // The line was continued, check if this line is a comment with an escaped new line.
            pair(many0(line_space), opt(comment)),
        )(span)?;

        if let Some(comment) = comment {
            // Line continuations at the end of a comment are not actually line continuations.
            if comment.ends_with('\\') {
                span.diag(
                    Diagnostic::builder(DiagnosticKind::BadEscape)
                        .label("this backslash is part of a comment", span.offset() - 1),
                );
            }

            continued.append(&mut comment.chars().collect());
        }

        Ok((span, continued))
    }

    map(
        pair(
            map(many0(alt((many1(line_space), continued))), |trivia| {
                trivia.into_iter().flatten().collect::<String>()
            }),
            opt(comment),
        ),
        |(mut trivia, comment)| {
            // Include the trailing comment.
            trivia.extend(comment);

            trivia
        },
    )(span)
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
        assert_debug_snapshot!(tests::parse_fail(line_ending, "\r"), @r###"
        (
            ParseError {
                location: 1,
                notes: [],
            },
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
        )
        "###);
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

    #[test]
    fn test_trivia() {
        // It's fine if there's nothing to parse.
        assert_debug_snapshot!(tests::parse_ok(trivia, "", ""), @r###"
        (
            [],
            "",
        )
        "###);
        assert_debug_snapshot!(tests::parse_ok(trivia, "foo", "foo"), @r###"
        (
            [],
            "",
        )
        "###);

        // Just space.
        assert_debug_snapshot!(tests::parse_ok(trivia, " \t ", ""), @r###"
        (
            [],
            " \t ",
        )
        "###);

        // Just a comment.
        assert_debug_snapshot!(tests::parse_ok(trivia, "# foo", ""), @r###"
        (
            [],
            "# foo",
        )
        "###);

        // Allow comments to have line continuations if they're not preceded by one.
        assert_debug_snapshot!(tests::parse_ok(trivia, "#foo \\\n", "\n"), @r###"
        (
            [],
            "#foo \\",
        )
        "###);

        // Comments without line continuations are okay.
        assert_debug_snapshot!(tests::parse_ok(trivia, " \\\n#foo", ""), @r###"
        (
            [],
            " #foo",
        )
        "###);

        // Warn when the comment tries to have a line continuation.
        assert_debug_snapshot!(tests::parse_ok(trivia, " \\\n#foo \\\n", "\n"), @r###"
        (
            [
                (
                    "shebling::bad_escape",
                    SourceSpan {
                        offset: SourceOffset(
                            9,
                        ),
                        length: 0,
                    },
                ),
            ],
            " #foo \\",
        )
        "###);
    }
}
