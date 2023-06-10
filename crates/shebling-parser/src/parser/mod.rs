#[cfg(test)]
mod tests;

mod quoted;
mod word;

use crate::{
    error::ParseError,
    span::{offset, ParseDiags, ParseSpan},
};
use quoted::single_quoted;

use nom::{
    branch::alt,
    bytes::complete::take_till,
    character::complete::{char, satisfy},
    combinator::{cut, map, opt, peek, recognize},
    error::context,
    sequence::{preceded, tuple},
    Finish,
};
use shebling_ast::*;
use shebling_diagnostic::{Diagnostic, DiagnosticKind};

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
// endregion

pub(crate) fn parse(source_code: &str, file_path: &str) {
    // HACK: When reporting errors, add a newline to the end of the source
    // so that miette can highlight the last character.

    use miette::Report;
    use std::sync::Arc;

    let diags = ParseDiags::new();

    match single_quoted(ParseSpan::new(source_code, &diags)).finish() {
        Ok((span, res)) => {
            println!("OK SPAN {:#?}", span);
            println!("OK RES {:#?}", res);

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
            // let diags = err.diags();
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
            println!(
                "{:?}",
                Report::new(err).with_source_code(Arc::clone(&source_code))
            );
        }
    }
}
