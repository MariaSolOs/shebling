use super::*;

pub(super) fn bats_test(span: Span) -> ParseResult<Function> {
    // Read the test header.
    let (span, mut header) = preceded(
        pair(tag("@test "), trivia),
        peek(recognize_string(is_not("\n"))),
    )(span)?;

    // The test name is everything before the last ' {'.
    if let Some(header_len) = header.rfind(" {") {
        header.truncate(header_len);
    } else {
        return context("invalid test name!", fail)(span);
    }

    // Parse the test body defined after the truncated header.
    let (span, body) = preceded(
        pair(take(header.len()), trivia),
        context("invalid test body!", brace_group),
    )(span)?;

    Ok((span, Function::new(header.trim_end(), body)))
}

pub(super) fn function(span: Span) -> ParseResult<Function> {
    fn parens(span: Span) -> ParseResult<()> {
        swallow(pair(
            terminated(char('('), trivia),
            terminated(
                alt((char(')'), |span| {
                    // No need for parameter lists.
                    let (span, (_, range)) = ranged(is_not("\n){"))(span)?;
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                            .label("parameter list", range)
                            .help("Just use '()' and refer to the parameters as $1, $2..."),
                    );

                    char(')')(span)
                })),
                trivia,
            ),
        ))(span)
    }

    fn with_keyword(span: Span) -> ParseResult<String> {
        // Read 'function' followed by the name.
        let (span, name) = preceded(
            pair(token(Keyword::Function), many1(line_space)),
            recognize_string(many1(alt((
                satisfy(|c: char| c.is_ascii_alphanumeric()),
                one_of("_:+?-./^@,[]*=!"),
            )))),
        )(span)?;

        let (span, (trivia, parens, body_follows)) =
            tuple((trivia, opt(parens), followed_by(one_of("{("))))(span)?;

        // Without parentheses, we need space before the function's body.
        if trivia.is_empty() && parens.is_none() && body_follows {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                    .range(&span)
                    .help("Add a space or new line between the function name and body."),
            );
        }

        Ok((span, name))
    }

    fn without_keyword(span: Span) -> ParseResult<String> {
        // Read the name, making sure it isn't 'time'.
        let (span, name) = verify(
            recognize_string(many1(alt((
                satisfy(|c: char| c.is_ascii_alphanumeric()),
                one_of("_:+?-./^@,"),
            )))),
            |name: &String| name != "time",
        )(span)?;

        let (span, _) = pair(trivia, parens)(span)?;

        Ok((span, name))
    }

    map(
        separated_pair(
            context(
                "invalid function signature!",
                alt((with_keyword, without_keyword)),
            ),
            pair(
                multi_trivia,
                peek(context(
                    "expected a '{' or '(' opening the function's body!",
                    one_of("{("),
                )),
            ),
            alt((brace_group, subshell)),
        ),
        |(name, body)| Function::new(name, body),
    )(span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_bats_test() {
        // These tests are the ones that ShellCheck uses because idek what a bats test is.
        assert_parse!(
            bats_test("@test 'can parse' {\n  true\n}"),
            Function::new("'can parse'", tests::pipeline("true"))
        );
        assert_parse!(
            bats_test("@test random text !(@*$Y&! {\n  true\n}"),
            Function::new("random text !(@*$Y&!", tests::pipeline("true"))
        );
        assert_parse!(
            bats_test("@test foo { bar { baz {\n  true\n}"),
            Function::new("foo { bar { baz", tests::pipeline("true"))
        );
        assert_parse!(bats_test("@test foo \n{\n true\n}") => Err(
            (1, 7),
            Notes: [((1, 7), "invalid test name")]
        ));
    }

    #[test]
    fn test_function() {
        // Function identifiers are more flexible than regular ones.
        assert_parse!(
            function(":() { foo; }"),
            Function::new(":", tests::pipeline("foo"))
        );
        assert_parse!(
            function("function []!() { foo; }"),
            Function::new("[]!", tests::pipeline("foo"))
        );

        // The body can be a subshell.
        assert_parse!(
            function("foo() (bar)"),
            Function::new("foo", tests::pipeline("bar"))
        );

        // No need for parameter lists.
        assert_parse!(
            function("foo(x, y) { bar; }"),
            Function::new("foo", tests::pipeline("bar")),
            [((1, 5), (1, 9), ParseDiagnosticKind::NotShellCode)]
        );

        // When using the keyword, there needs to be space before the body
        // if there are no parens.
        assert_parse!(
            function("function foo{ bar; }"),
            Function::new("foo", tests::pipeline("bar")),
            [((1, 13), ParseDiagnosticKind::MissingSpace)]
        );

        // Missing the body's opening delimiter.
        assert_parse!(function("function foo\n  bar;") => Err(
            (2, 3),
            Notes: [((2, 3), "expected a '{' or '('")]
        ));

        // Only horizontal whitespace is allowed after 'function'.
        assert_parse!(function("function\nfoo() { bar; }") => Err(
            (1, 9),
            Notes: [((1, 1), "invalid function signature")]
        ));
    }
}
