use super::*;

pub(super) fn bats_test(span: Span) -> ParseResult<BatsTest> {
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

    Ok((span, BatsTest::new(header.trim_end(), body)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_bats_test() {
        // These tests are the ones that ShellCheck uses because idek what a bats test is.
        assert_parse!(
            bats_test("@test 'can parse' {\n  true\n}") => "",
            BatsTest::new("'can parse'", tests::lit_pipeline("true"))
        );
        assert_parse!(
            bats_test("@test random text !(@*$Y&! {\n  true\n}") => "",
            BatsTest::new("random text !(@*$Y&!", tests::lit_pipeline("true"))
        );
        assert_parse!(
            bats_test("@test foo { bar { baz {\n  true\n}") => "",
            BatsTest::new("foo { bar { baz", tests::lit_pipeline("true"))
        );
        assert_parse!(bats_test("@test foo \n{\n true\n}") => Err(
            (1, 7),
            Notes: [((1, 7), "invalid test name")]
        ));
    }
}
