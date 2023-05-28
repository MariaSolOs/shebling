mod cursor;
mod diagnostic;
mod word;

use shebling_ast::*;

use cursor::Cursor;
use diagnostic::ParseDiagnosticKind;
use word::word;

pub(crate) type ParseResult<'a, T> = Result<(Cursor<'a>, T), Cursor<'a>>;

pub(crate) fn map<P1, P2, R1, R2>(parser: P1, mapper: P2) -> impl Fn(Cursor) -> ParseResult<R2>
where
    P1: Fn(Cursor) -> ParseResult<R1>,
    P2: Fn(R1) -> R2,
{
    move |cursor| parser(cursor).map(|(cursor, res)| (cursor, mapper(res)))
}

pub fn parse(file_path: &str, source: &str) {
    use std::sync::Arc;

    let cursor = Cursor::new(source);
    match word(cursor) {
        Ok((cursor, res)) => {
            let (remaining, diags) = cursor.into_remaining_diags();

            println!("OK RESULT {:#?}", res);
            println!("OK REMAINING {:#?}", remaining);

            // HACK: When reporting errors, we add a newline to the end of the source
            // so that miette can highlight the last character.
            let source = Arc::new(miette::NamedSource::new(
                file_path,
                source.to_owned() + "\n",
            ));

            diags.into_iter().for_each(|err| {
                println!(
                    "{:?}",
                    miette::Report::new(err).with_source_code(Arc::clone(&source))
                );
            });
        }
        Err(cursor) => {
            println!("ERR CURSOR {:#?}", cursor);
        }
    }
}
