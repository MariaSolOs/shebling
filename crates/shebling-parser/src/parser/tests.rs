use super::*;

use miette::Diagnostic;
use pretty_assertions::assert_str_eq;
use std::fmt;

pub(crate) fn parse_fail<P, R>(
    parser: P,
    source: &str,
) -> (ParseError, Vec<(String, miette::SourceSpan)>)
where
    P: Fn(ParseSpan) -> ParseResult<R>,
    R: fmt::Debug,
{
    // Parse and ensure it succeeds.
    let diags = ParseDiags::new();
    let err = parser(ParseSpan::new(source, &diags))
        .finish()
        .expect_err("Parsing should fail");

    (err, summarize_diags(diags))
}

pub(crate) fn parse_ok<P, R>(
    parser: P,
    source: &str,
    remaining: &str,
) -> (Vec<(String, miette::SourceSpan)>, R)
where
    P: Fn(ParseSpan) -> ParseResult<R>,
{
    // Parse and ensure it succeeds.
    let diags = ParseDiags::new();
    let (span, res) = parser(ParseSpan::new(source, &diags))
        .finish()
        .expect("Parsing should succeed");

    // Check the remaining bit.
    assert_str_eq!(span.fragment(), remaining);

    (summarize_diags(diags), res)
}

fn summarize_diags(diags: ParseDiags) -> Vec<(String, miette::SourceSpan)> {
    diags
        .into_iter()
        .map(|diag| {
            let code = diag
                .code()
                .expect("Diagnostic should have a code")
                .to_string();
            let span = *diag.span();

            (code, span)
        })
        .collect()
}
