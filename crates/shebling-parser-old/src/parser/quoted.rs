use super::*;
use crate::ParseContext;

pub(super) const DOUBLE_ESCAPABLE: &str = "\\\"$`";

pub(super) fn backquoted(span: Span) -> ParseResult<Term> {
    escaping_backquoted(false)(span)
}

pub(super) fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

pub(super) fn double_quoted(span: Span) -> ParseResult<DoubleQuoted> {
    delimited(
        char('"'),
        map(
            many0(alt((
                into(dollar_exp),
                into(double_quoted_lit),
                into(escaping_backquoted(true)),
            ))),
            DoubleQuoted::new,
        ),
        context("expected ending double quote!", char('"')),
    )(span)
}

fn double_quoted_lit(span: Span) -> ParseResult<String> {
    fn double_quoted_escape(span: Span) -> ParseResult<String> {
        let (span, (escaped, range)) = ranged(escaped(DOUBLE_ESCAPABLE))(span)?;

        if escaped.len() >= 2 {
            // Some non-special character was escaped.
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape).label(
                    "this character has no special behavior when escaped inside double quotes",
                    range,
                ),
            );
        }

        Ok((span, escaped))
    }

    fn lonely_dollar(span: Span) -> ParseResult<String> {
        // Parse the literal dollar.
        let (span, (dollar, range)) = ranged(char('$'))(span)?;

        // Sometimes people want to escape a dollar inside double quotes and end the string
        // to achieve that. But that's ugly, they should just use a backslash.
        let (span, next_char) = opt(peek(preceded(
            pair(char('"'), opt(char('"'))),
            alt((satisfy(|c| c.is_ascii_alphanumeric()), one_of("_?!#-@"))),
        )))(span)?;
        if let Some(next_char) = next_char {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::MissingEscape)
                    .label("is this supposed to be a literal dollar?", range)
                    .help(format!(
                        "Instead of \" .. $\"{0}, use \" .. \\${0} .. \"",
                        next_char
                    )),
            );
        }

        Ok((span, dollar.into()))
    }

    alt((
        map(
            many1(alt((
                double_quoted_escape,
                // Parse until we hit an escapable char or a unicode quote.
                recognize_string(is_not(DOUBLE_ESCAPABLE)),
            ))),
            |sgmt| sgmt.concat(),
        ),
        lonely_dollar,
    ))(span)
}

pub(super) fn escaped<'a>(can_escape: &'static str) -> impl FnMut(Span<'a>) -> ParseResult<String> {
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

fn escaping_backquoted(escape_double_quotes: bool) -> impl Fn(Span) -> ParseResult<Term> {
    fn backquote(span: Span) -> ParseResult<()> {
        alt((swallow(char('`')), |span| {
            let (span, (_, range)) = ranged(char('´'))(span)?;
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("forward tick", range)
                    .help("For command expansion, use backticks (``)."),
            );

            Ok((span, ()))
        }))(span)
    }

    move |span| {
        // Parse the quotes and the command string.
        let (span, cmd_start) = preceded(backquote, position)(span)?;
        let (span, cmd) = many0(alt((
            escaped(if escape_double_quotes {
                "$`\\\""
            } else {
                "$`\\"
            }),
            recognize_string(is_not("\\`´")),
        )))(span)?;
        let (span, _) = context("expected ending backtick!", backquote)(span)?;
        let cmd = cmd.concat();

        // Subparse the quoted content.
        let cmd_span = unsafe {
            Span::new_from_raw_offset(
                cmd_start.location_offset(),
                cmd_start.location_line(),
                &cmd,
                ParseContext::new(),
            )
        };
        let (cmd_span, cmd) = all_consuming(preceded(multi_trivia, term))(cmd_span)?;
        span.extra.extend_diags(cmd_span.extra);

        Ok((span, cmd))
    }
}

pub(super) fn line_continuation(span: Span) -> ParseResult<()> {
    swallow(pair(backslash, newline))(span)
}

pub(super) fn single_quoted(span: Span) -> ParseResult<String> {
    // Parse the opening quote and the string.
    let (span, string) = preceded(char('\''), recognize_string(take_till(|c| c == '\'')))(span)?;

    // Check that the ending quote isn't escaped.
    let last_char = string.chars().last().unwrap_or_default();
    if last_char == '\\' {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                .label("the backslash before this quote is literal", &span)
                .help("Wanna escape a single quote? 'Let'\\''s do it correctly'"),
        );
    }

    // Verify the closing quote.
    let (span, (_, range)) =
        ranged(cut(context("expected ending single quote!", char('\''))))(span)?;

    // Apostrophe check.
    let (span, alphabetic_follows) = followed_by(satisfy(|c| c.is_ascii_alphabetic()))(span)?;
    if alphabetic_follows && last_char.is_ascii_alphabetic() {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::UnclosedString)
                .label("this apostrophe terminated the string!", range)
                .help("Try escaping the apostrophe, 'it'\\''s done like this!'"),
        );
    }

    Ok((span, string))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_backquoted() {
        // Lint for the wrong kind of backquote.
        assert_parse!(
            backquoted("´foo´"),
            tests::pipeline("foo").into(),
            [
                ((1, 1), (1, 2), ParseDiagnosticKind::SusToken),
                ((1, 5), (1, 6), ParseDiagnosticKind::SusToken)
            ]
        );

        // Make sure lints from the subparser are included.
        assert_parse!(
            backquoted("`foo\\nbar`"),
            tests::pipeline("foonbar").into(),
            [((1, 5), (1, 7), ParseDiagnosticKind::BadEscape)]
        );
    }

    #[test]
    fn test_backslash() {
        // An actual backslash.
        assert_parse!(backslash("\\"), '\\');

        // A forward slash != backslash.
        assert_parse!(backslash("/") => Err(1, 1));
    }

    #[test]
    fn test_double_quoted() {
        // Can be empty.
        assert_parse!(double_quoted("\"\""), DoubleQuoted::new(vec![]));

        // Can have complex stuff like dollar expansions.
        assert_parse!(
            double_quoted("\"${ foo; }\""),
            DoubleQuoted::new(vinto![DollarExp::CmdExpansion(
                tests::pipeline("foo").into()
            )])
        );

        // Double quotes can be escaped in backticks.
        assert_parse!(
            double_quoted("\"`\"foo\"`\""),
            DoubleQuoted::new(vec![DoubleQuotedSgmt::BackQuoted(
                Pipeline::new(vinto![SimpleCmd::new(
                    Some(Word::new(vinto![DoubleQuoted::new(vec![
                        DoubleQuotedSgmt::Lit("foo".into())
                    ])])),
                    vec![],
                    vec![]
                )])
                .into()
            )])
        );
    }

    #[test]
    fn test_double_quoted_lit() {
        // Only certain characters can be escaped inside double quotes.
        assert_parse!(double_quoted_lit("foo\\$"), "foo$");
        assert_parse!(
            double_quoted_lit("foo\\n"),
            "foo\\n",
            [((1, 4), (1, 6), ParseDiagnosticKind::BadEscape)]
        );

        // A lonely dollar is fine,
        assert_parse!(double_quoted_lit("$"), "$");
        // ...but emit warning when it looks like the "literal dollar hack".
        assert_parse!(
            double_quoted_lit("$\"1") => "\"1",
            "$",
            [((1, 1), (1, 2), ParseDiagnosticKind::MissingEscape)]
        );

        // Can't be empty.
        assert_parse!(double_quoted_lit("") => Err(1, 1));
    }

    #[test]
    fn test_single_quoted() {
        // A correctly written single quoted string.
        assert_parse!(single_quoted("'foo bar'"), "foo bar");

        // An empty string is also fine.
        assert_parse!(single_quoted("''"), "");

        // The ending quote looks like a failed escape.
        assert_parse!(
            single_quoted("'foo\\'"),
            "foo\\",
            [((1, 6), ParseDiagnosticKind::BadEscape)]
        );

        // Apostrophe that ends the string.
        assert_parse!(
            single_quoted("'let's'") => "s'",
            "let",
            [((1, 5), (1, 6), ParseDiagnosticKind::UnclosedString)]
        );

        // Make sure both quotes are present.
        assert_parse!(single_quoted("foo'") => Err(1, 1));
        assert_parse!(single_quoted("'foo") => Err((1, 5), Notes: [((1, 5), "expected ending single quote")]));
    }
}
