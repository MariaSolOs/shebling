use super::*;

const DOUBLE_UNIQUOTES: &str = "\u{201C}\u{201D}\u{2033}\u{2036}";
const SINGLE_UNIQUOTES: &str = "\u{2018}\u{2019}";

fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

fn double_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(DOUBLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("double quote", range));

    Ok((span, quote))
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
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::UnclosedString)
                    .label("did you forget to close this string?", start)
                    .label(
                        "this is an ending quote, but the next char looks kinda sus.",
                        end,
                    ),
            );
        }
    }

    Ok((span, SingleQuoted::new(string)))
}

fn single_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(SINGLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("single quote", range));

    Ok((span, quote))
}

// region: Utilities.
fn is_sus_char_after_quote(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | '%')
}
// endregion

#[cfg(test)]
mod tests {
    use super::*;

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
