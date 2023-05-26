#[cfg(test)]
mod tests;

use std::str::Chars;

use shebling_ast::{ControlOp, RedirOp, Span, Spanned};

// TODO: Document types and function logic.
#[derive(Debug, PartialEq)]
pub enum Token {
    Comment(String),
    ControlOp(ControlOp),
    RedirOp(RedirOp),
    Word(Vec<Spanned<WordSgmt>>),
}

#[derive(Debug, PartialEq)]
pub enum WordSgmt {
    DoubleQuoted {
        sgmts: Vec<Spanned<WordSgmt>>,
        translated: bool,
        closed: bool,
    },
    Lit(String),
    SingleQuoted {
        string: String,
        ansi_c_quoted: bool,
        closed: bool,
    },
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum LexerDiagnostic {
    #[error("CRLF line ending!")]
    #[diagnostic(
        code(shebling::cr_lf),
        help("Try running the script through tr -d '\\r'.")
    )]
    CrLf(#[label("literal carriage return")] usize),

    #[error("unclosed {1}!")]
    #[diagnostic(code(shebling::unclosed_word))]
    UnclosedWord(#[label("missing closing '{2}'")] usize, &'static str, char),
}

struct Lexer<'a> {
    chars: Chars<'a>,
    source_len: usize,
    diags: Vec<LexerDiagnostic>,
}

// TODO: Inline the functions I just use once.

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars(),
            source_len: source.len(),
            diags: vec![],
        }
    }

    fn tokenize(mut self) -> (Vec<Spanned<Token>>, Vec<LexerDiagnostic>) {
        let mut tokens = vec![];

        // TODO: Process shebang.

        self.blanks();
        while let Some(token) = self.token() {
            tokens.push(token);
            self.blanks();
        }

        // Make sure we read everything.
        assert!(self.chars.next().is_none());

        (tokens, self.diags)
    }

    // region: Individual tokenizers.
    fn blanks(&mut self) {
        self.eat_while(|c| matches!(c, ' ' | '\t'));
    }

    fn control_op(&mut self) -> Token {
        let op = match self
            .bump()
            .expect("tokenize() should have peeked something")
        {
            '&' => {
                if self.bumped('&') {
                    ControlOp::AndIf
                } else {
                    ControlOp::And
                }
            }
            ';' => {
                if self.bumped(';') {
                    ControlOp::DSemi
                } else {
                    ControlOp::Semi
                }
            }
            '|' => {
                if self.bumped('|') {
                    ControlOp::OrIf
                } else {
                    ControlOp::Or
                }
            }
            '\n' => ControlOp::Newline,
            _ => unreachable!("tokenize() should have peeked an operator prefix."),
        };

        Token::ControlOp(op)
    }

    fn double_quoted(&mut self) -> WordSgmt {
        // Check if this is a translated string.
        let translated = self.bumped('$');

        assert!(self.bumped('"'));

        let mut sgmts = vec![];
        while let Some(c) = self.peek() {
            let sgmt_start = self.position();

            let sgmt = if let Some(lit) = self.lit("\\\"$`", "$`\"") {
                WordSgmt::Lit(lit)
            } else {
                match c {
                    // Reached the end of the string.
                    '"' => break,
                    '$' => {
                        self.bump();

                        match self.peek() {
                            // This means that the string is unclosed so we don't know
                            // what the dollar is supposed to be. Anyway...
                            None => WordSgmt::Lit('$'.into()),
                            _ => todo!(),
                        }
                    }
                    '`' => todo!(),
                    _ => unreachable!("lit() should have consumed '{}'", c),
                }
            };

            sgmts.push(Spanned::new(sgmt, self.capture_span(sgmt_start)));
        }

        WordSgmt::DoubleQuoted {
            sgmts,
            translated,
            closed: if let Some(c) = self.bump() {
                assert!(c == '"');
                true
            } else {
                self.diags.push(LexerDiagnostic::UnclosedWord(
                    self.position(),
                    "double quoted string",
                    '"',
                ));
                false
            },
        }
    }

    fn lit(&mut self, can_escape: &str, stop_with: &str) -> Option<String> {
        let mut lit = String::new();

        while let Some(c) = self.peek_bump(|c| !stop_with.contains(c)) {
            match c {
                '\\' => match self.bump() {
                    Some('\n') | None => {
                        // Either a line continuation or a lonely backslash. Either way,
                        // don't add anything to the literal.
                    }
                    Some(escaped) => {
                        if !can_escape.contains(escaped) {
                            lit.push('\\');
                        }
                        lit.push(escaped);
                    }
                },
                _ => lit.push(c),
            };
        }

        if lit.is_empty() {
            None
        } else {
            Some(lit)
        }
    }

    fn redir_op(&mut self) -> Token {
        let op = match self
            .bump()
            .expect("tokenize() should have peeked something")
        {
            '<' => match self.peek_bump(|c| matches!(c, '<' | '&' | '>')) {
                Some('<') => {
                    if self.bumped('-') {
                        RedirOp::DLessDash
                    } else {
                        RedirOp::DLess
                    }
                }
                Some('&') => RedirOp::LessAnd,
                Some('>') => RedirOp::LessGreat,
                _ => RedirOp::Less,
            },
            '>' => match self.peek_bump(|c| matches!(c, '|' | '>' | '&')) {
                Some('|') => RedirOp::Clobber,
                Some('>') => RedirOp::DGreat,
                Some('&') => RedirOp::GreatAnd,
                _ => RedirOp::Great,
            },
            _ => unreachable!("tokenize() should have peeked an operator prefix."),
        };

        Token::RedirOp(op)
    }

    fn single_quoted(&mut self) -> WordSgmt {
        // Check if this is an ANSI-C quoted string.
        let ansi_c_quoted = self.bumped('$');

        assert!(self.bumped('\''));
        let string = self.eat_while(|c| c != '\'');

        WordSgmt::SingleQuoted {
            string,
            ansi_c_quoted,
            closed: if let Some(c) = self.bump() {
                assert!(c == '\'');
                true
            } else {
                self.diags.push(LexerDiagnostic::UnclosedWord(
                    self.position(),
                    "single quoted string",
                    '\'',
                ));
                false
            },
        }
    }

    fn token(&mut self) -> Option<Spanned<Token>> {
        if let Some(c) = self.peek() {
            let mut start = self.position();

            let token = match c {
                '&' | ';' | '|' | '\n' => self.control_op(),
                '\r' if self.peek2().is_some_and(|c| c == '\n') => {
                    // Special case for CRLF line endings, which we
                    // leniently read as new lines.
                    self.diags.push(LexerDiagnostic::CrLf(start));

                    self.bump();
                    start = self.position();
                    self.bump();

                    Token::ControlOp(ControlOp::Newline)
                }
                '<' | '>' => self.redir_op(),
                '#' => Token::Comment(self.eat_while(|c| c != '\n')),
                _ => {
                    if let Some(word) = self.word() {
                        word
                    } else {
                        // This can happen if the word is just a line continuation.
                        // Do check that we bumped the cursor.
                        assert!(start < self.position());

                        return None;
                    }
                }
            };

            Some(Spanned::new(token, self.capture_span(start)))
        } else {
            None
        }
    }

    fn word(&mut self) -> Option<Token> {
        let mut word = vec![];

        while let Some(c) = self.peek() {
            let sgmt_start = self.position();

            let sgmt = match c {
                '"' => self.double_quoted(),
                '\'' => self.single_quoted(),
                '$' => {
                    match self.peek2() {
                        Some('"') => self.double_quoted(),
                        Some('\'') => self.single_quoted(),
                        None => {
                            // This is technically undefined behavior, but we'll just treat it as
                            // a literal dollar.
                            self.bump();
                            WordSgmt::Lit('$'.into())
                        }
                        _ => todo!(),
                    }
                }
                _ => {
                    if let Some(lit) = self.lit("|&;<>()$`\\\"' \t\n", "#|&;<>()$`\"' \t\r\n") {
                        WordSgmt::Lit(lit)
                    } else {
                        break;
                    }
                }
            };

            word.push(Spanned::new(sgmt, self.capture_span(sgmt_start)));
        }

        if word.is_empty() {
            None
        } else {
            Some(Token::Word(word))
        }
    }
    // endregion

    // region: "Cursor" utilities.
    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn bumped(&mut self, expecting: char) -> bool {
        self.peek_bump(|c| c == expecting).is_some()
    }

    fn eat_while(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut eaten = String::new();

        while let Some(c) = self.peek_bump(&condition) {
            eaten.push(c);
        }

        eaten
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek2(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn peek_bump(&mut self, condition: impl Fn(char) -> bool) -> Option<char> {
        let c = self.peek()?;

        if condition(c) {
            self.bump()
        } else {
            None
        }
    }

    fn position(&self) -> usize {
        self.source_len - self.chars.as_str().len()
    }

    fn capture_span(&self, start: usize) -> Span {
        assert!(start < self.position());

        Span::new(start, self.position())
    }
    // endregion
}

pub fn tokenize_source(source: &str) -> (Vec<Spanned<Token>>, Vec<LexerDiagnostic>) {
    Lexer::new(source).tokenize()
}
