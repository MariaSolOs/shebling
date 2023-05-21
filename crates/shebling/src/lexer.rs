// TODO: Remove this after development.
#![allow(dead_code)]

use std::{fmt, iter::Peekable, str::Chars};

// TODO: Document types.

struct Span {
    start: usize,
    end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug)]
pub(crate) struct Spanned<T>(T, Span);

#[derive(Debug)]
pub(crate) enum ControlOp {
    /// `&`
    And,
    /// `&&`
    AndIf,
    /// `;;`
    DSemi,
    /// `\n`
    Newline,
    /// `|`
    Or,
    /// `|&`
    OrAnd,
    /// `||`
    OrIf,
    /// `;`
    Semi,
}

#[derive(Debug)]
pub(crate) enum Token {
    Comment(String),
    ControlOp(ControlOp),
}

pub(crate) struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    prev_char: char,
    chars_read: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
            prev_char: char::default(),
            chars_read: 0,
        }
    }

    pub(crate) fn read_tokens(mut self) -> Vec<Spanned<Token>> {
        let mut tokens = Vec::new();

        // Eat any starting whitespace.
        self.eat_while(|c| matches!(c, ' ' | '\t'));

        while let Some(c) = self.bump() {
            // Minus one because we just read the starting character.
            let start = self.chars_read - 1;

            let token = match c {
                '&' | ';' | '|' | '\n' => Token::ControlOp(self.control_op()),
                '#' => Token::Comment(self.comment()),
                _ => todo!(),
            };

            tokens.push(Spanned(
                token,
                Span {
                    start,
                    end: self.chars_read,
                },
            ));

            // Consume trailing whitespace.
            // TODO: Warn when finding "\r\n"s.
            self.eat_while(|c| matches!(c, ' ' | '\t'));
        }

        tokens
    }

    fn comment(&mut self) -> String {
        self.eat_while(|c| !matches!(c, '\r' | '\n'))
    }

    fn control_op(&mut self) -> ControlOp {
        match self.prev_char {
            '&' => {
                if self.peek_bump(|c| c == &'&').is_some() {
                    ControlOp::AndIf
                } else {
                    ControlOp::And
                }
            }
            ';' => {
                if self.peek_bump(|c| c == &';').is_some() {
                    ControlOp::DSemi
                } else {
                    ControlOp::Semi
                }
            }
            '|' => {
                if self.peek_bump(|c| c == &'|').is_some() {
                    ControlOp::OrIf
                } else if self.peek_bump(|c| c == &'&').is_some() {
                    ControlOp::OrAnd
                } else {
                    ControlOp::Or
                }
            }
            '\n' => ControlOp::Newline,
            _ => unreachable!("An operator prefix should have been read."),
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev_char = c;
        self.chars_read += 1;

        Some(c)
    }

    fn peek_bump(&mut self, condition: impl Fn(&char) -> bool) -> Option<char> {
        let c = self.chars.peek()?;

        if condition(c) {
            self.bump()
        } else {
            None
        }
    }

    fn eat_while(&mut self, condition: impl Fn(&char) -> bool) -> String {
        let mut eaten = String::new();

        while let Some(c) = self.peek_bump(&condition) {
            eaten.push(c);
        }

        eaten
    }
}
