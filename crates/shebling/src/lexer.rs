// TODO: Remove this after development.
#![allow(dead_code)]

use std::{iter::Peekable, str::Chars};

// TODO: Document types.

#[derive(Debug)]
struct Span {
    start: usize,
    end: usize,
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
            let start = self.chars_read - 1;

            let token = match c {
                '&' | ';' | '|' | '\n' => Token::ControlOp(self.control_op()),
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
            self.eat_while(|c| matches!(c, ' ' | '\t'));
        }

        tokens
    }

    fn control_op(&mut self) -> ControlOp {
        match self.prev_char {
            '&' => match self.peek() {
                Some('&') => {
                    self.bump();
                    ControlOp::AndIf
                }
                _ => ControlOp::And,
            },
            ';' => match self.peek() {
                Some(';') => {
                    self.bump();
                    ControlOp::DSemi
                }
                _ => ControlOp::Semi,
            },
            '|' => match self.peek() {
                Some('|') => {
                    self.bump();
                    ControlOp::OrIf
                }
                Some('&') => {
                    self.bump();
                    ControlOp::OrAnd
                }
                _ => ControlOp::Or,
            },
            '\n' => ControlOp::Newline,
            _ => unreachable!("An operator prefix should have been read."),
        }
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(&char) -> bool) {
        while let Some(c) = self.peek() {
            if predicate(c) {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev_char = c;
        self.chars_read += 1;

        Some(c)
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }
}
