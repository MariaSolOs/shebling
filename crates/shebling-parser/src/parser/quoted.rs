use super::*;

fn single_quoted(span: ParseSpan) -> ParseResult<String> {
    // Parse the opening quote and the string.
    // let (span, string) = preceded(char('\''), take_till(|c| c == '\''))(span)?;

    // // Check that the ending quote isn't escaped.
    // let last_char = string.chars().last().unwrap_or_default();
    // if last_char == '\\' {
    //     span.extra.diag(
    //         ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
    //             .label("the backslash before this quote is literal", &span)
    //             .help("Wanna escape a single quote? 'Let'\\''s do it correctly'"),
    //     );
    // }

    // // Verify the closing quote.
    // let (span, (_, range)) =
    //     ranged(cut(context("expected ending single quote!", char('\''))))(span)?;

    // // Apostrophe check.
    // let (span, alphabetic_follows) = followed_by(satisfy(|c| c.is_ascii_alphabetic()))(span)?;
    // if alphabetic_follows && last_char.is_ascii_alphabetic() {
    //     span.extra.diag(
    //         ParseDiagnostic::builder(ParseDiagnosticKind::UnclosedString)
    //             .label("this apostrophe terminated the string!", range)
    //             .help("Try escaping the apostrophe, 'it'\\''s done like this!'"),
    //     );
    // }

    // Ok((span, string))
    todo!()
}
