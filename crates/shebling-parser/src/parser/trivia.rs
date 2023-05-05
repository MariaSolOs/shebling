use super::*;

const UNISPACES: &str = "\u{A0}\u{200B}";

fn carriage_return(span: Span) -> ParseResult<char> {
    let (span, (cr, range)) = ranged(char('\r'))(span)?;
    span.extra.diag(
        ParseDiagnostic::builder(ParseDiagnosticKind::UnexpectedToken)
            .label("carriage return", range)
            .help("Try running the script through tr -d '\\r'."),
    );

    Ok((span, cr))
}

fn comment(span: Span) -> ParseResult<String> {
    recognize_string(pair(char('#'), is_not("\r\n")))(span)
}

pub(super) fn line_ending(span: Span) -> ParseResult<char> {
    // TODO: readPendingHereDocs
    preceded(opt(carriage_return), newline)(span)
}

pub(super) fn line_space(span: Span) -> ParseResult<char> {
    alt((one_of(" \t"), |span| {
        let (span, (_, range)) = ranged(one_of(UNISPACES))(span)?;
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("unicode space", range),
        );

        Ok((span, ' '))
    }))(span)
}

pub(super) fn multi_trivia(span: Span) -> ParseResult<String> {
    map(separated_list0(line_ending, trivia), |trivia| {
        trivia.join("\n")
    })(span)
}

pub(super) fn multi_trivia1(span: Span) -> ParseResult<String> {
    context(
        "expected whitespace!",
        verify(multi_trivia, |trivia: &String| !trivia.is_empty()),
    )(span)
}

pub(super) fn trivia(span: Span) -> ParseResult<String> {
    fn continued(span: Span) -> ParseResult<Vec<char>> {
        let (span, (mut continued, comment)) = preceded(
            line_continuation,
            // The line was continued, check if this line is a comment with an escaped new line.
            pair(many0(line_space), opt(comment)),
        )(span)?;

        if let Some(comment) = comment {
            // Line continuations at the end of a comment are not actually line continuations.
            if comment.ends_with('\\') {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                        .label("this backslash is part of a comment", &span),
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

pub(super) fn trivia1(span: Span) -> ParseResult<String> {
    context(
        "expected whitespace!",
        verify(trivia, |trivia: &str| !trivia.is_empty()),
    )(span)
}

pub(super) fn whitespace(span: Span) -> ParseResult<char> {
    alt((line_space, carriage_return, line_ending))(span)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_carriage_return() {
        // Check parsed CR and warning.
        assert_parse!(
            carriage_return("\r") => "",
            '\r',
            [((1, 1), (1, 2), ParseDiagnosticKind::UnexpectedToken)]
        );

        // Not a CR.
        assert_parse!(carriage_return("\n") => Err(1, 1));
    }

    #[test]
    fn test_comment() {
        // A Bash comment.
        assert_parse!(comment("# foo") => "", "# foo");

        // Make sure we stop at the end of a line.
        assert_parse!(comment("# foo\n") => "\n", "# foo");
        assert_parse!(comment("# foo\r\n") => "\r\n", "# foo");

        // Not a Bash comment.
        assert_parse!(comment("// foo") => Err(1, 1));
    }

    #[test]
    fn test_line_ending() {
        // A new line is fine.
        assert_parse!(line_ending("\n") => "", '\n');

        // Parse CRLF but warn about the carriage return.
        assert_parse!(
            line_ending("\r\n") => "",
            '\n',
            [((1, 1), (1, 2), ParseDiagnosticKind::UnexpectedToken)]
        );

        // Missing the new line.
        assert_parse!(line_ending("\r") => Err(
            (1, 2),
            Diags: [((1, 1), (1, 2), ParseDiagnosticKind::UnexpectedToken)]
        ));

        // Not a new line at all.
        assert_parse!(line_ending("\t") => Err(1, 1));
    }

    #[test]
    fn test_line_space() {
        // Spaces and tabs are fine.
        assert_parse!(line_space(" ") => "", ' ');
        assert_parse!(line_space("\t") => "", '\t');

        // Parse unicode spaces but emit warning.
        assert_parse!(line_space("\u{A0}") => "", ' ', [((1, 1), (1, 2), ParseDiagnosticKind::Unichar)]);

        // Not a space.
        assert_parse!(line_space("\n") => Err(1, 1));
    }

    #[test]
    fn test_multi_trivia() {
        // It's okay if some lines are empty.
        assert_parse!(multi_trivia("\n \n") => "", "\n \n");

        // Multiple comments.
        assert_parse!(multi_trivia("# foo \n #bar\n") => "", "# foo \n #bar\n");

        // Handle line continuations.
        assert_parse!(multi_trivia("\\\n# foo") => "", "# foo");

        // Stop when we should.
        assert_parse!(multi_trivia(" foo") => "foo", " ");
    }

    #[test]
    fn test_trivia() {
        // It's fine if there's nothing to parse.
        assert_parse!(trivia("") => "", "");
        assert_parse!(trivia("foo") => "foo", "");

        // Just space.
        assert_parse!(trivia(" \t ") => "", " \t ");

        // Just a comment.
        assert_parse!(trivia("# foo") => "", "# foo");

        // Allow comments to have line continuations if they're not preceded by one.
        assert_parse!(trivia("# foo \\\n") => "\n", "# foo \\");

        // Comments without line continuations are okay.
        assert_parse!(trivia(" \\\n# foo") => "", " # foo");

        // Warn when the comment tries to have a line continuation.
        assert_parse!(
            trivia(" \\\n# foo \\\n") => "\n",
            " # foo \\",
            [((2, 8), ParseDiagnosticKind::BadEscape)]
        );
    }

    #[test]
    fn test_trivia1() {
        // Non-empty space.
        assert_parse!(trivia1(" ") => "", " ");

        // The parsed output cannot be empty.
        assert_parse!(trivia1("") => Err((1, 1), Notes: [((1, 1), "expected whitespace")]));
        assert_parse!(trivia1("foo") => Err((1, 1), Notes: [((1, 1), "expected whitespace")]));
    }
}
