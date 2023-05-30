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
            Diagnostic::builder(DiagnosticKind::SusChar)
                .label("this apostrophe terminates the string!", quote)
                .help("Try escaping the apostrophe, 'it'\\''s done like this!'"),
        );
    }

    Ok((span, string))
}
