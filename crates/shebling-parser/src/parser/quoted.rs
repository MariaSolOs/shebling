use super::*;

const DOUBLE_UNIQUOTES: &str = "\u{201C}\u{201D}\u{2033}\u{2036}";
const SINGLE_UNIQUOTES: &str = "\u{2018}\u{2019}";

fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

fn double_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(DOUBLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::new(ParseDiagnosticKind::Unichar).label("double quote", range));

    Ok((span, quote))
}

pub(super) fn line_continuation(span: Span) -> ParseResult<()> {
    swallow(pair(backslash, newline))(span)
}

fn single_uniquote(span: Span) -> ParseResult<char> {
    let (span, (quote, range)) = ranged(one_of(SINGLE_UNIQUOTES))(span)?;
    span.extra
        .diag(ParseDiagnostic::new(ParseDiagnosticKind::Unichar).label("single quote", range));

    Ok((span, quote))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_double_uniquote() {
        // A legit uniquote.
        assert_parse!(double_uniquote("\u{201C}") => "", '\u{201C}', [((1, 1), (1, 2), ParseDiagnosticKind::Unichar)]);

        // Not a uniquote.
        assert_parse!(double_uniquote("\"") => Err(1, 1));
    }

    #[test]
    fn test_single_uniquote() {
        // A legit uniquote.
        assert_parse!(single_uniquote("\u{2018}") => "", '\u{2018}', [((1, 1), (1, 2), ParseDiagnosticKind::Unichar)]);

        // Not a uniquote.
        assert_parse!(single_uniquote("'") => Err(1, 1));
    }
}
