use super::*;
use pretty_assertions::assert_eq;

#[test]
fn lit_with_escapes() {
    for (source, expected) in [
        ("foo\\ bar", "foo bar"), // Escaped blank
        ("\\$0", "$0"),           // Escaped special character
        ("foo\\\nbar", "foobar"), // Line continuation
    ] {
        assert_eq!(
            tokenize_ok(source),
            vec![Spanned::new(
                Token::Word(vec![Spanned::new(
                    WordSgmt::Lit(expected.into()),
                    Span::new(0, source.len())
                )]),
                Span::new(0, source.len())
            )]
        );
    }
}

#[test]
fn operator_prefixes() {
    // Make sure that we correctly handle some operators being prefixes of others.
    assert_eq!(
        tokenize_ok(">>&"),
        vec![
            Spanned::new(Token::RedirOp(RedirOp::DGreat), Span::new(0, 2)),
            Spanned::new(Token::ControlOp(ControlOp::And), Span::new(2, 3))
        ]
    );
    assert_eq!(
        tokenize_ok("|||||"),
        vec![
            Spanned::new(Token::ControlOp(ControlOp::OrIf), Span::new(0, 2)),
            Spanned::new(Token::ControlOp(ControlOp::OrIf), Span::new(2, 4)),
            Spanned::new(Token::ControlOp(ControlOp::Or), Span::new(4, 5)),
        ]
    );
}

fn tokenize_ok(source: &str) -> Vec<Spanned<Token>> {
    let (tokens, diags) = tokenize_source(source);
    assert_eq!(diags.len(), 0);
    tokens
}
