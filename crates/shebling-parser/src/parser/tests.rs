use crate::ast::*;

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
        ::pretty_assertions::assert_eq!(start.line, $line1, "Start column: {}", start.line);
        ::pretty_assertions::assert_eq!(start.column, $col1, "Start column: {}", start.column);
        ::pretty_assertions::assert_eq!(end.line, $line2, "End line: {}", end.line);
        ::pretty_assertions::assert_eq!(end.column, $col2, "End column: {}", end.column);
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

// region: Shared constructors for common (and verbose) AST objects.
pub(crate) fn arith_number(n: &str) -> ArithExpansion {
    ArithExpansion::new(vec![Lit::new(n).into()])
}
// endregion
