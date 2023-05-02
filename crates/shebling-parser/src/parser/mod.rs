use crate::{
    ast::*,
    diagnostic::{ParseDiagnostic, ParseDiagnosticKind},
    error::ParseError,
    Range, Span,
};
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, newline, one_of, satisfy},
    combinator::{consumed, cut, into, map, not, opt, peek, recognize, value, verify},
    error::context,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use nom_locate::position;

#[cfg(test)]
#[macro_use]
mod tests;

mod expansion;

mod quoted;
use quoted::{line_continuation, single_quoted};

mod token;
use token::token;

mod trivia;
use trivia::whitespace;

mod word;
use word::identifier;

type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

// TODO: Remove this test function.
#[allow(unused_variables, unused_imports)]
pub(crate) fn test(file_path: impl AsRef<str>, source_code: &str) {
    use crate::source_to_span;
    use miette::Report;
    use std::sync::Arc;

    // let diags: Vec<ParseDiagnostic> = expansion::arith_seq(source_to_span(source_code))
    //     .finish()
    //     .unwrap()
    //     .0
    //     .extra
    //     .take_diags();

    // let source_code = Arc::new(miette::NamedSource::new(
    //     file_path,
    //     source_code.to_owned() + "\n",
    // ));
    // for diag in diags {
    //     println!(
    //         "{:?}",
    //         Report::new(diag).with_source_code(Arc::clone(&source_code))
    //     );
    // }

    // let err = single_quoted(source_to_span(source_code))
    //     .finish()
    //     .unwrap_err();
    // let source_code = Arc::new(miette::NamedSource::new(
    //     file_path,
    //     source_code.to_owned() + "\n",
    // ));
    // println!(
    //     "{:?}",
    //     Report::new(err).with_source_code(Arc::clone(&source_code))
    // );
    // for diag in err.diags {
    //     println!(
    //         "{:?}",
    //         Report::new(diag).with_source_code(Arc::clone(&source_code))
    //     );
    // }
}

// region: Shared utility parsers.
fn followed_by<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<bool>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(opt(peek(parser)), |res| res.is_some())
}

fn lit<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<Lit>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
    R: Into<String>,
{
    map(parser, |res| Lit::new(res.into()))
}

fn ranged<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<(R, Range)>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(tuple((position, parser, position)), |(start, res, end)| {
        (res, Range::new(start, end))
    })
}

fn recognize_string<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<String>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(recognize(parser), |span| (*span.fragment()).into())
}

fn swallow<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<()>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    value((), parser)
}
// endregion
