use super::*;

pub(super) fn identifier(span: Span) -> ParseResult<String> {
    recognize_string(pair(
        // Make sure that the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(span)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        // Valid identifiers.
        assert_parse!(identifier("foo") => "", "foo");
        assert_parse!(identifier("_foo0") => "", "_foo0");

        // Identifiers can't start with a number.
        assert_parse!(identifier("0foo") => Err(1, 1));

        // Cannot be empty.
        assert_parse!(identifier("") => Err(1, 1));
    }
}
