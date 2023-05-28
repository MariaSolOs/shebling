use crate::ast::*;
use wec::vinto;

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
        let (start, end) = ($diag.range().start(), $diag.range().end());
        let (line, column) = (start.line(), start.column());
        ::pretty_assertions::assert_eq!(line, $line1, "Actual start line: {}", line);
        ::pretty_assertions::assert_eq!(column, $col1, "Actual start column: {}", column);
        let (line, column) = (end.line(), end.column());
        ::pretty_assertions::assert_eq!(line, $line2, "Actual end line: {}", line);
        ::pretty_assertions::assert_eq!(column, $col2, "Actual end column: {}", column);
    };
}

macro_rules! assert_parse {
    (
        $parser:ident($source:literal),
        $res:expr
        $(, [ $( (($line1:literal, $col1:literal), $(($line2:literal, $col2:literal),)? $kind:path) ),+ ] )?
    ) => {
        assert_parse!(
            $parser($source) => "",
            $res
            $(, [ $( (($line1, $col1), $(($line2, $col2),)? $kind) ),+ ] )?
        );
    };

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
        let mut diags = span.extra.take_diags().into_iter();
        $($(
            let diag = diags.next().expect("Expected a parser diagnostic.");
            assert_diag_eq!(&diag, (($line1, $col1), $(($line2, $col2),)? $kind));
        )+)?
        let last_diag = diags.next();
        assert!(last_diag.is_none(), "There's a diag left: {:#?}", last_diag);
    };

    ($parser:ident($source:literal) => Err($line:literal, $col:literal)) => {
        assert_parse!($parser($source) => Err(($line, $col),));
    };

    ($parser:ident($source:literal) => Err(
        ($line:literal, $col:literal),
        $( Notes: [ $( (($line1:literal, $col1:literal), $note:literal) ),+ ] $(,)? )?
        $( Diags: [ $( (($line2:literal, $col2:literal), $(($line3:literal, $col3:literal),)? $kind:path) ),+ ] )?
    )) => {
        let err = $parser($crate::source_to_span($source))
            .finish()
            .expect_err("Parser should fail.");

        // Check the error location.
        let (line, column) = (err.line(), err.column());
        ::pretty_assertions::assert_eq!(line, $line, "Actual line: {}", line);
        ::pretty_assertions::assert_eq!(column, $col, "Actual column: {}", column);

        let mut notes = err.notes().into_iter();
        $($(
            // For flexibility, just check that the notes have the expected messages.
            let note = notes.next().expect("Expected a parser note.");
            let (line, column, note) = (note.line(), note.column(), note.note());
            ::pretty_assertions::assert_eq!(line, $line1, "Actual line: {}", line);
            ::pretty_assertions::assert_eq!(column, $col1, "Actual column: {}", column);
            assert!(note.contains($note), "Expected \"{}\" to contain \"{}\"", note, $note);
        )+)?
        let last_note = notes.next();
        assert!(last_note.is_none(), "There's a note left: {:#?}", last_note);

        // Check the diagnostics.
        let mut diags = err.diags().into_iter();
        $($(
            let diag = diags.next().expect("Expected a parser diagnostic.");
            assert_diag_eq!(diag, (($line2, $col2), $(($line3, $col3),)? $kind));
        )+)?
        let last_diag = diags.next();
        assert!(last_diag.is_none(), "There's a diag left: {:#?}", last_diag);
    };
}

// region: Shared constructors for common (and verbose) AST objects.
pub(crate) fn arith_number(n: &str) -> ArithTerm {
    ArithTerm::Expansion(vec![WordSgmt::Lit(n.into())])
}

pub(crate) fn cmd(lit: &str) -> SimpleCmd {
    SimpleCmd::new(Some(word(lit)), vec![], vec![])
}

pub(crate) fn pipeline(lit: &str) -> Pipeline {
    Pipeline::new(vinto![cmd(lit)])
}

pub(crate) fn word(lit: &str) -> Word {
    Word::new(vec![WordSgmt::Lit(lit.into())])
}

pub(crate) fn var(ident: &str) -> SubscriptedVar {
    SubscriptedVar::new(ident, vec![])
}
// endregion
