#[cfg(test)]
#[macro_use]
mod tests;

mod command;
mod expansion;
mod function;
mod quoted;
mod redirection;
mod token;
mod trivia;
mod word;

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, tag_no_case, take, take_till},
    character::complete::{alpha1, alphanumeric1, anychar, char, digit1, newline, one_of, satisfy},
    combinator::{
        all_consuming, consumed, cut, eof, fail, into, map, not, opt, peek, recognize, value,
        verify,
    },
    error::context,
    multi::{many0, many1, many_till, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use nom_locate::position;
use wec::vinto;

use crate::{
    ast::*,
    diagnostic::{ParseDiagnostic, ParseDiagnosticKind},
    error::ParseError,
    location::{Range, Span},
};
use command::*;
use expansion::*;
use function::*;
use quoted::*;
use redirection::*;
use token::*;
use trivia::*;
use word::*;

type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

// TODO: Remove this test function.
#[allow(unused_variables, unused_imports)]
pub(crate) fn test(file_path: impl AsRef<str>, source_code: &str) {
    use crate::source_to_span;
    use miette::Report;
    use std::sync::Arc;

    match arith_seq(source_to_span(source_code)).finish() {
        Ok((span, res)) => {
            println!("OK SPAN {:#?}", span);
            println!("OK RES {:#?}", res);
            let diags: Vec<ParseDiagnostic> = span.extra.take_diags();

            let source_code = Arc::new(miette::NamedSource::new(
                file_path,
                source_code.to_owned() + "\n",
            ));
            for diag in diags {
                println!(
                    "{:?}",
                    Report::new(diag).with_source_code(Arc::clone(&source_code))
                );
            }
        }
        Err(err) => {
            println!("ERR {:#?}", err);
            let diags = err.diags();
            let source_code = Arc::new(miette::NamedSource::new(
                file_path,
                source_code.to_owned() + "\n",
            ));
            for diag in diags {
                println!(
                    "{:?}",
                    Report::new(diag.clone()).with_source_code(Arc::clone(&source_code))
                );
            }
            println!(
                "{:?}",
                Report::new(err).with_source_code(Arc::clone(&source_code))
            );
        }
    }
}

// region: Shared utility parsers.
fn followed_by<'a, P, R>(parser: P) -> impl FnMut(Span<'a>) -> ParseResult<bool>
where
    P: FnMut(Span<'a>) -> ParseResult<R>,
{
    map(opt(peek(parser)), |res| res.is_some())
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
