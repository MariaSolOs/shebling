use super::*;

pub(super) const DOUBLE_ESCAPABLE: &str = "\\\"$`";
pub(super) const DOUBLE_UNIQUOTES: &str = "\u{201C}\u{201D}\u{2033}\u{2036}";
pub(super) const SINGLE_UNIQUOTES: &str = "\u{2018}\u{2019}";

pub(super) fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

pub(super) fn double_quoted(span: Span) -> ParseResult<DoubleQuoted> {
    // Parse the quotes and string segments.
    let (span, start) = tag("\"")(span)?;
    let (span, sgmts) = many0(alt((
        into(dollar_exp),
        into(double_quoted_lit),
        // TODO into(backquoted(true)),
        into(lit(double_uniquote)),
    )))(span)?;
    let (span, end) = cut(context("Expected end of double quoted string!", tag("\"")))(span)?;

    // Does this look like an unclosed string?
    let (span, sus_next_char) =
        followed_by(alt((satisfy(is_sus_char_after_quote), one_of("$\""))))(span)?;
    if sus_next_char {
        if let Some(DoubleQuotedSgmt::Lit(first_lit)) = sgmts.first() {
            if !first_lit.value().starts_with('\n')
                && sgmts.iter().any(|sgmt| {
                    if let DoubleQuotedSgmt::Lit(lit) = sgmt {
                        lit.value().contains('\n')
                    } else {
                        false
                    }
                })
            {
                report_unclosed_string(&span, start, end);
            }
        }
    }

    Ok((span, DoubleQuoted::new(sgmts)))
}

fn double_quoted_lit(span: Span) -> ParseResult<Lit> {
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

    fn lonely_dollar(span: Span) -> ParseResult<Lit> {
        // Parse the literal dollar.
        let (span, (dollar, range)) = ranged(lit(char('$')))(span)?;

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

        Ok((span, dollar))
    }

    alt((
        map(
            many1(alt((
                double_quoted_escape,
                // Parse until we hit an escapable char or a unicode quote.
                recognize_string(is_not(&*format!(
                    "{}{}",
                    DOUBLE_ESCAPABLE, DOUBLE_UNIQUOTES
                ))),
            ))),
            |sgmt| Lit::new(sgmt.concat()),
        ),
        lonely_dollar,
    ))(span)
}

pub(super) fn double_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(DOUBLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("double quote", range));

    Ok((span, quote))
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

pub(super) fn line_continuation(span: Span) -> ParseResult<()> {
    swallow(pair(backslash, newline))(span)
}

pub(super) fn single_quoted(span: Span) -> ParseResult<SingleQuoted> {
    // Parse the opening quote and the string.
    let (span, (start, string)) = pair(
        tag("'"),
        map(
            many0(alt((
                into(single_uniquote),
                recognize_string(is_not(&*format!("'{}", SINGLE_UNIQUOTES))),
            ))),
            |sgmt| sgmt.concat(),
        ),
    )(span)?;

    // Check that the ending quote isn't escaped.
    let last_char = string.chars().last().unwrap_or_default();
    if last_char == '\\' {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                .label("the backslash before this quote is literal", &span)
                .help("Wanna escape a single quote? 'Let'\\''s do it correctly'"),
        )
    }

    // Verify the closing quote.
    let (span, (end, range)) =
        ranged(cut(context("expected ending single quote!", tag("'"))))(span)?;

    // Is the string unclosed?
    let (span, next_char) = opt(peek(satisfy(|c| is_sus_char_after_quote(c) || c == '\'')))(span)?;
    if let Some(next_char) = next_char {
        if next_char.is_ascii_alphabetic() && last_char.is_ascii_alphabetic() {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::UnclosedString)
                    .label("this apostrophe terminated the string!", range)
                    .help("Try escaping the apostrophe, 'it'\\''s done like this!'"),
            );
        } else if !string.starts_with('\n') && string.contains('\n') {
            report_unclosed_string(&span, start, end);
        }
    }

    Ok((span, SingleQuoted::new(string)))
}

pub(super) fn single_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(SINGLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("single quote", range));

    Ok((span, quote))
}

// region: Utilities.
fn is_sus_char_after_quote(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | '%')
}

fn report_unclosed_string(span: &Span, start: Span, end: Span) {
    span.extra.diag(
        ParseDiagnostic::builder(ParseDiagnosticKind::UnclosedString)
            .label("did you forget to close this string?", start)
            .label(
                "this is an ending quote, but the next char looks kinda sus.",
                end,
            ),
    )
}
// endregion

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_double_quoted() {
        // TODO
    }

    #[test]
    fn test_double_quoted_lit() {
        // Only certain characters can be escaped inside double quotes.
        assert_parse!(double_quoted_lit("foo\\$") => "", Lit::new("foo$"));
        assert_parse!(
            double_quoted_lit("foo\\n") => "",
            Lit::new("foo\\n"),
            [((1, 4), (1, 6), ParseDiagnosticKind::BadEscape)]
        );

        // A lonely dollar is fine,
        assert_parse!(double_quoted_lit("$") => "", Lit::new("$"));
        // ...but emit warning when it looks like the "literal dollar hack".
        assert_parse!(
            double_quoted_lit("$\"1") => "\"1",
            Lit::new("$"),
            [((1, 1), (1, 2), ParseDiagnosticKind::MissingEscape)]
        );

        // Can't be empty.
        assert_parse!(double_quoted_lit("") => Err(1, 1));
    }

    #[test]
    fn test_double_uniquote() {
        // A legit uniquote.
        assert_parse!(
            double_uniquote("\u{201C}") => "",
            '\u{201C}',
            [((1, 1), (1, 2), ParseDiagnosticKind::Unichar)]
        );

        // Not a uniquote.
        assert_parse!(double_uniquote("\"") => Err(1, 1));
    }

    #[test]
    fn test_single_quoted() {
        // A correctly written single quoted string.
        assert_parse!(single_quoted("'foo bar'") => "", SingleQuoted::new("foo bar"));

        // An empty string is also fine.
        assert_parse!(single_quoted("''") => "", SingleQuoted::new(""));

        // Warn when finding a uniquote.
        assert_parse!(
            single_quoted("'let’s'") => "",
            SingleQuoted::new("let’s"),
            [((1, 5), (1, 6), ParseDiagnosticKind::Unichar)]
        );

        // The ending quote looks like a failed escape.
        assert_parse!(
            single_quoted("'foo\\'") => "",
            SingleQuoted::new("foo\\"),
            [((1, 6), ParseDiagnosticKind::BadEscape)]
        );

        // Apostrophe that ends the string.
        assert_parse!(
            single_quoted("'let's'") => "s'",
            SingleQuoted::new("let"),
            [((1, 5), (1, 6), ParseDiagnosticKind::UnclosedString)]
        );

        // Multi-line string, but the next character looks sus.
        assert_parse!(
            single_quoted("'foo\n'bar") => "bar",
            SingleQuoted::new("foo\n"),
            [((1, 1), ParseDiagnosticKind::UnclosedString)]
        );

        // Make sure both quotes are present.
        assert_parse!(single_quoted("foo'") => Err(1, 1));
        assert_parse!(single_quoted("'foo") => Err((1, 5), Notes: [((1, 5), "expected ending single quote")]));
    }

    #[test]
    fn test_single_uniquote() {
        // A legit uniquote.
        assert_parse!(
            single_uniquote("\u{2018}") => "",
            '\u{2018}',
            [((1, 1), (1, 2), ParseDiagnosticKind::Unichar)]
        );

        // Not a uniquote.
        assert_parse!(single_uniquote("'") => Err(1, 1));
    }
}
