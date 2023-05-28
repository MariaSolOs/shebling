use super::*;

// fn lit(&mut self, can_escape: &str, stop_with: &str) -> Option<String> {
//     let mut lit = String::new();

//     while let Some(c) = self.peek_bump(|c| !stop_with.contains(c)) {
//         match c {
//             '\\' => match self.bump() {
//                 Some('\n') | None => {
//                     // Either a line continuation or a lonely backslash. Either way,
//                     // don't add anything to the literal.
//                 }
//                 Some(escaped) => {
//                     if !can_escape.contains(escaped) {
//                         lit.push('\\');
//                     }
//                     lit.push(escaped);
//                 }
//             },
//             _ => lit.push(c),
//         };
//     }

//     if lit.is_empty() {
//         None
//     } else {
//         Some(lit)
//     }
// }

fn single_quoted(mut cursor: Cursor) -> ParseResult<String> {
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

pub(crate) fn word(mut cursor: Cursor) -> ParseResult<Word> {
    let mut sgmts = vec![];

    while let Some(c) = cursor.peek() {
        let start = cursor.position();
        let sgmt;
        (cursor, sgmt) = match c {
            '\'' => map(single_quoted, |string| WordSgmt::SingleQuoted {
                string,
                ansi_c_quoted: false,
            })(cursor)?,
            '$' => {
                cursor.bump();

                match cursor.peek() {
                    Some('\'') => map(single_quoted, |string| WordSgmt::SingleQuoted {
                        string,
                        ansi_c_quoted: true,
                    })(cursor)?,
                    _ => todo!(),
                }
            }
            _ => break,
        };
        sgmts.push(Spanned::new(sgmt, Span::new(start, cursor.position())));
    }

    if sgmts.is_empty() {
        Err(cursor)
    } else {
        Ok((cursor, Word::new(sgmts)))
    }
}
