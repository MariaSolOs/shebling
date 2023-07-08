#[cfg(test)]
mod tests;

mod expansion;
mod quoted;
mod token;
mod trivia;
mod word;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till},
    character::complete::{anychar, char, newline, one_of, satisfy},
    combinator::{consumed, cut, map, not, opt, peek, recognize, value},
    error::context,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use shebling_ast::*;
use shebling_diagnostic::{Diagnostic, DiagnosticKind};

use crate::{
    error::ParseError,
    span::{offset, ParseDiags, ParseSpan},
};
use quoted::{double_quoted, line_continuation, single_quoted};
use token::token;
use trivia::whitespace;

/// Result of a `shebling` parser.
pub(crate) type ParseResult<'a, R> = nom::IResult<ParseSpan<'a>, R, ParseError>;

// region: Utility parsers.
fn peeked<'a, P, R>(parser: P) -> impl FnMut(ParseSpan<'a>) -> ParseResult<bool>
where
    P: Fn(ParseSpan<'a>) -> ParseResult<R>,
{
    map(opt(peek(parser)), |res| res.is_some())
}

fn recognize_string<'a, P, R>(parser: P) -> impl FnMut(ParseSpan<'a>) -> ParseResult<String>
where
    P: Fn(ParseSpan<'a>) -> ParseResult<R>,
{
    map(recognize(parser), |span| span.fragment().into())
}

fn spanned<'a, P, R>(parser: P) -> impl FnMut(ParseSpan<'a>) -> ParseResult<Spanned<R>>
where
    P: FnMut(ParseSpan<'a>) -> ParseResult<R>,
{
    map(tuple((offset, parser, offset)), |(start, res, end)| {
        Spanned::new(res, Span::new(start, end))
    })
}

fn swallow<'a, P, R>(parser: P) -> impl FnMut(ParseSpan<'a>) -> ParseResult<()>
where
    P: FnMut(ParseSpan<'a>) -> ParseResult<R>,
{
    value((), parser)
}
// endregion

pub(crate) fn parse(source_code: &str, file_path: &str) {
    // HACK: When reporting errors, add a newline to the end of the source
    // so that miette can highlight the last character.

    use miette::Report;
    use std::sync::Arc;

    let diags = ParseDiags::new();

    let res = single_quoted(ParseSpan::new(source_code, &diags)).finish();
    let source_code = Arc::new(miette::NamedSource::new(
        file_path,
        source_code.to_owned() + "\n",
    ));

    match res {
        Ok((span, res)) => {
            println!("OK SPAN {:#?}", span);
            println!("OK RES {:#?}", res);
        }
        Err(err) => {
            println!("ERR {:#?}", err);
            println!(
                "{:?}",
                Report::new(err).with_source_code(Arc::clone(&source_code))
            );
        }
    }

    for diag in diags {
        println!(
            "{:?}",
            Report::new(diag).with_source_code(Arc::clone(&source_code))
        );
    }
}
