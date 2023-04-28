use super::*;

fn backslash(span: Span) -> ParseResult<char> {
    char('\\')(span)
}

pub(crate) fn line_continuation(span: Span) -> ParseResult<()> {
    swallow(pair(backslash, newline))(span)
}
