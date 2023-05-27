// TODO: Remove this.
#![allow(dead_code)]

mod cursor;
mod diagnostic;

use cursor::Cursor;
use diagnostic::ParseDiagnosticKind;

fn single_quoted(mut cursor: Cursor) -> Result<(Cursor, String), Cursor> {
    if !cursor.bumped('\'') {
        return Err(cursor);
    }

    let string = cursor.eat_while(|c| c != '\'');

    if !cursor.bumped('\'') {
        cursor.diag(
            ParseDiagnosticKind::UnclosedString("single quoted"),
            "missing closing '",
        );
    }

    Ok((cursor, string))
}

pub fn parse(source: &str) {
    let cursor = Cursor::new(source);
    println!("{:#?}", single_quoted(cursor));
}
