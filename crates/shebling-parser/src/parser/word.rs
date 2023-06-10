use super::*;

fn word_sgmt(span: ParseSpan) -> ParseResult<WordSgmt> {
    alt((map(spanned(single_quoted), WordSgmt::SingleQuoted),))(span)
}
