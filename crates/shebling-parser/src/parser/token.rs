use super::*;

/// Trait for [Token] types which can be directly parsed from a [Span].
pub(super) trait ParseToken
where
    Self: Token,
{
    fn parse_token(self, span: Span) -> ParseResult<Self> {
        // This isn't recursing, it calls the generic function below.
        // By having a separate inner function, override implementations
        // can still use the default parse_token.
        parse_token(self)(span)
    }
}

/// Creates a parser for the given token based on its `token()` pattern.
fn parse_token<'a, T: Token>(token: T) -> impl FnMut(Span<'a>) -> ParseResult<T> {
    value(token, tag(token.token()))
}

impl ParseToken for BinOp {}

impl ParseToken for ControlOp {
    fn parse_token<'a>(self, mut span: Span<'a>) -> ParseResult<'a, Self> {
        // Special case for ';' since we don't want to mix it up with ';;'.
        if let ControlOp::Semi = self {
            (span, _) = not(token(ControlOp::DSemi))(span)?;
        }

        parse_token(self)(span)
    }
}

impl ParseToken for Keyword {
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        let keyword = self.token();

        let (span, word) = recognize_string(tag_no_case(keyword))(span)?;

        let (span, missing_space) = if self == Keyword::Function {
            // Special case for 'function', since they can only be followed
            // (and have to) by horizontal whitespace.
            map(followed_by(line_space), |space| !space)(span)?
        } else {
            // Keywords followed by '[', '#', '!', or ':' need a space between them.
            followed_by(one_of("[#!:"))(span)?
        };
        if missing_space {
            span.extra
                .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(&span));
        }

        // Reserved words must be at the end of the file or followed by space or a metacharacter.
        let (span, _) = peek(alt((
            swallow(eof),
            swallow(multi_trivia1),
            swallow(one_of(";()<>&|")),
        )))(span)?;

        if word != keyword {
            context("keywords should be lower-cased!", fail)(span)
        } else {
            // Return the keyword.
            Ok((span, self))
        }
    }
}

impl ParseToken for RedirOp {}

impl ParseToken for UnOp {}

/// Creates a parser for the given [ParseToken].
pub(super) fn token<'a, P: ParseToken>(token: P) -> impl FnMut(Span<'a>) -> ParseResult<P> {
    move |span| {
        let (span, _) = token.parse_token(span)?;

        Ok((span, token))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_control_op_token() {
        // Semicolons are a special case.
        let mut parser = token(ControlOp::Semi);
        assert_parse!(parser(";") => "", ControlOp::Semi);
        assert_parse!(parser(";;") => Err(1, 1));
    }

    #[test]
    fn test_parse_keyword_token() {
        // Need space after certain characters.
        let mut parser = token(Keyword::If);
        assert_parse!(parser("if[") => Err((1, 3), Diags: [((1, 3), ParseDiagnosticKind::MissingSpace)]));

        // Have to be followed by the end of the file, space, or a metacharacter.
        assert_parse!(parser("if") => "", Keyword::If);
        assert_parse!(parser("if ") => " ", Keyword::If);
        assert_parse!(parser("if;") => ";", Keyword::If);

        // Fail with bad casing.
        assert_parse!(parser("If") => Err((1, 3), Notes: [((1, 3), "keywords should be lower-cased")]));

        // Make sure that 'function' is always followed by horizontal whitespace.
        parser = token(Keyword::Function);
        assert_parse!(parser("function foo") => " foo", Keyword::Function);
        assert_parse!(
            parser("function") => "",
            Keyword::Function,
            [((1, 9), ParseDiagnosticKind::MissingSpace)]
        );
    }
}
