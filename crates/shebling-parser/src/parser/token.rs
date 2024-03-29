use super::*;

/// Trait for [Token] types which can be directly parsed from a [ParseSpan].
pub(super) trait ParseToken
where
    Self: Token,
{
    fn parse_token(self, span: ParseSpan) -> ParseResult<Self> {
        // This isn't recursing, it calls the generic function below.
        // By having a separate inner function, override implementations
        // can still use the default parse_token.
        parse_token(self)(span)
    }
}

/// Creates a parser for the given token based on its `token()` pattern.
fn parse_token<'a, T: Token>(token: T) -> impl FnMut(ParseSpan<'a>) -> ParseResult<T> {
    value(token, tag(token.token()))
}

impl ParseToken for BinOp {}

impl ParseToken for UnOp {}

/// Returns a parser for the given [ParseToken].
pub(super) fn token<'a, P: ParseToken>(token: P) -> impl FnMut(ParseSpan<'a>) -> ParseResult<P> {
    move |span| {
        let (span, _) = token.parse_token(span)?;

        Ok((span, token))
    }
}
