use crate::{
    ast::*,
    diagnostic::{ParseDiagnostic, ParseDiagnosticKind},
    error::ParseError,
    Range, Span,
};
// We import nom's parsers here so that we don't need to import
// them in every file.
#[allow(unused_imports)]
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, newline, one_of, satisfy},
    combinator::{consumed, cut, into, map, not, opt, peek, recognize, value, verify},
    error::context,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish,
};
use nom_locate::position;

#[cfg(test)]
#[macro_use]
mod tests {
    macro_rules! assert_diag_eq {
        ($diag:expr, (($line:literal, $col:literal), $kind:path)) => {
            assert_diag_eq!($diag, (($line, $col), ($line, $col), $kind))
        };

        ($diag:expr, (($line1:literal, $col1:literal), ($line2:literal, $col2:literal), $kind:path)) => {
            // Check that the codes match.
            let actual_code = ::miette::Diagnostic::code($diag)
                .expect("Diagnostic should have a code.")
                .to_string();
            let expected_code = ::miette::Diagnostic::code(&$kind)
                .expect("Diagnostic should have a code.")
                .to_string();
            ::pretty_assertions::assert_str_eq!(actual_code, expected_code);

            // Check the range coordinates.
            let $crate::Range { start, end } = $diag.range();
            ::pretty_assertions::assert_eq!(start.line, $line1);
            ::pretty_assertions::assert_eq!(start.column, $col1);
            ::pretty_assertions::assert_eq!(end.line, $line2);
            ::pretty_assertions::assert_eq!(end.column, $col2);
        };
    }

    macro_rules! assert_parse {
        (
            $parser:ident($source:literal) => $unparsed:literal,
            $res:expr
            $(, [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $kind:path) ),+ ] )?
        ) => {
            let (span, res) = $parser($crate::source_to_span($source))
                .finish()
                .expect("Parser should succeed.");

            // Verify the result.
            ::pretty_assertions::assert_eq!(res, $res);

            // Verify the unparsed content.
            ::pretty_assertions::assert_str_eq!(*span.fragment(), $unparsed);

            // Verify the diagnostics.
            $($(
                for diag in span.extra.take_diags() {
                    assert_diag_eq!(&diag, (($line1, $col1), $(($line2, $col2),)? $kind));
                }
            )+)?
        };

        ($parser:ident($source:literal) => Err($line:literal, $col:literal)) => {
            assert_parse!($parser($source) => Err(($line, $col), Notes: [], Diags: []));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Notes: [ $( (($line1:literal, $col1:literal), $note:literal) ),+ ]
        )) => {
            assert_parse!($parser($source) => Err(
                ($line, $col),
                Notes: [ $( (($line1, $col1), $note) ),+ ],
                Diags: []
            ));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Diags: [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $kind:path) ),+ ]
        )) => {
            assert_parse!($parser($source) => Err(
                ($line, $col),
                Notes: [],
                Diags: [ $( (($line1, $col1), $(($line2, $col2),)? $kind) ),+ ]
            ));
        };

        ($parser:ident($source:literal) => Err(
            ($line:literal, $col:literal),
            Notes: [ $( (($line1:literal, $col1:literal), $note:literal) ),* ],
            Diags: [ $( (($line2:literal, $col2:literal), $(($line3:literal, $col3:literal),)? $kind:path) ),* ]
        )) => {
            let err = $parser($crate::source_to_span($source))
                .finish()
                .expect_err("Parser should fail.");

            // Check the error location.
            ::pretty_assertions::assert_eq!(err.line(), $line);
            ::pretty_assertions::assert_eq!(err.column(), $col);

            // For flexibility, just check that the notes have the expected messages.
            let mut notes = err.notes().into_iter();
            $(
                let note = notes.next().expect("Expected a parser note.");
                ::pretty_assertions::assert_eq!(note.line(), $line1);
                ::pretty_assertions::assert_eq!(note.column(), $col1);
                let note = note.note();
                assert!(note.contains($note), "Expected \"{}\" to contain \"{}\"", note, $note);
            )*
            let last_note = notes.next();
            assert!(last_note.is_none(), "There's a note left: {:#?}", last_note);

            // Check the diagnostics.
            $(
                for diag in err.diags() {
                    assert_diag_eq!(diag, (($line2, $col2), $(($line3, $col3),)? $kind));
                }
            )*
        };
    }
}

mod expansion;

mod quoted;
use quoted::line_continuation;

mod token;

mod trivia;

type ParseResult<'a, R> = nom::IResult<Span<'a>, R, ParseError>;

// TODO: Remove this test function.
#[allow(unused_variables, unused_imports)]
pub(crate) fn test(file_path: impl AsRef<str>, source_code: &str) {
    use crate::source_to_span;
    use miette::Report;
    use std::sync::Arc;

    // let diags: Vec<ParseDiagnostic> = single_quoted(source_to_span(source_code))
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
