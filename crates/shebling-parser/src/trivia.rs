use super::*;

const UNISPACES: &str = "\u{A0}\u{200B}";

fn comment(span: Span) -> ParseResult<String> {
    recognize_string(pair(char('#'), is_not("\r\n")))(span)
}

fn line_space(span: Span) -> ParseResult<char> {
    alt((one_of(" \t"), |span| {
        let (span, (_, range)) = ranged(one_of(UNISPACES))(span)?;

        span.extra
            .report(ParseDiagnostic::Unichar("space".into(), range));

        Ok((span, ' '))
    }))(span)
}

fn trivia(span: Span) -> ParseResult<String> {
    fn continued(span: Span) -> ParseResult<Vec<char>> {
        let (span, (mut continued, comment)) = preceded(
            line_continuation,
            // The line was continued, check if this line is a comment with an escaped new line.
            pair(many0(line_space), opt(comment)),
        )(span)?;

        if let Some(comment) = comment {
            // Line continuations at the end of a comment are not actually line continuations.
            if comment.ends_with('\\') {
                span.extra.report(ParseDiagnostic::MisplacedChar(
                    "This backslash is part of a comment.".into(),
                    Range::from(&span),
                ));
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
