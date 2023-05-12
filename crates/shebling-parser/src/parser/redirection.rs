use super::*;

pub(super) fn redir(span: Span) -> ParseResult<Redir> {
    // Read the optional file descriptor.
    let (span, desc) = context(
        "invalid file descriptor!",
        alt((
            map(
                alt((
                    map(char('&'), |_| FileDesc::StdOutErr),
                    map(delimited(char('{'), identifier, char('}')), |ident| {
                        SubscriptedVar::new(ident, vec![]).into()
                    }),
                    map(digit1, |_| FileDesc::Number),
                )),
                Some,
            ),
            // Make sure the operator follows if we didn't parse a descriptor.
            map(peek(one_of("<>")), |_| None),
        )),
    )(span)?;

    // Parse the redirection operator and word.
    let (span, (op, word)) = preceded(
        context("expected a redirection operator!", peek(one_of("<>"))),
        alt((
            // Here strings:
            separated_pair(token(RedirOp::TLess), trivia, word),
            // TODO: readHereDoc
            // File descriptor duplication:
            separated_pair(
                alt((token(RedirOp::GreatAnd), token(RedirOp::LessAnd))),
                trivia,
                // The word can be a variable, or a digit optionally followed by a dash,
                // or a dash.
                map(
                    alt((
                        delimited(char('{'), identifier, char('}')),
                        recognize_string(pair(digit1, opt(char('-')))),
                        recognize_string(char('-')),
                    )),
                    |word| Word::new(vec![WordSgmt::Lit(word)]),
                ),
            ),
            // Redirection from/to a file.
            separated_pair(
                alt((
                    token(RedirOp::DGreat),
                    token(RedirOp::LessGreat),
                    token(RedirOp::GreatAnd),
                    token(RedirOp::LessAnd),
                    token(RedirOp::Clobber),
                    token(RedirOp::DLess),
                    token(RedirOp::Less),
                    token(RedirOp::Great),
                )),
                trivia,
                word,
            ),
        )),
    )(span)?;

    // Spacing and bye.
    let (span, _) = trivia(span)?;

    Ok((span, Redir::new(desc, op, word)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_redir() {
        // The file descriptor can be omitted.
        assert_parse!(
            redir(">foo"),
            Redir::new(None, RedirOp::Great, tests::word("foo"))
        );

        // Make sure that operators that are prefixed by others are correctly parsed.
        assert_parse!(
            redir("<< foo"),
            Redir::new(None, RedirOp::DLess, tests::word("foo"))
        );

        // Valid file descriptor duplications:
        assert_parse!(
            redir("{foo}>&1-"),
            Redir::new(
                Some(tests::var("foo").into()),
                RedirOp::GreatAnd,
                tests::word("1-"),
            )
        );
        assert_parse!(
            redir("{foo}>&{bar}"),
            Redir::new(
                Some(tests::var("foo").into()),
                RedirOp::GreatAnd,
                tests::word("bar"),
            )
        );

        // Invalid file descriptor.
        assert_parse!(redir("{$foo}<<bar") => Err(
            (1, 1),
            Notes: [((1, 1), "invalid file descriptor")]
        ));

        // Can't have a space before the operator.
        assert_parse!(redir("{foo} <<bar") => Err((1, 6), Notes: [((1, 6), "expected a redirection operator")]));

        // TODO: Add a here doc test.
    }
}
