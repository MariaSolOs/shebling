// TODO: Remove this after development.
#![allow(dead_code)]

use std::{fmt, str::Chars};

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
pub(crate) enum RedirOp {
    /// `>|`
    Clobber,
    /// `>>`
    DGreat,
    /// `<<`
    DLess,
    /// `<<-`
    DLessDash,
    /// `>`
    Great,
    /// `>&`
    GreatAnd,
    /// `<`
    Less,
    /// `<&`
    LessAnd,
    /// `<>`
    LessGreat,
    /// `<<<`
    TLess,
}

#[derive(Debug)]
pub(crate) enum Token {
    Comment(String),
    ControlOp(ControlOp),
    RedirOp(RedirOp),
}

pub(crate) struct Lexer<'a> {
    chars: Chars<'a>,
    prev_char: char,
    source_len: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars(),
            prev_char: char::default(),
            source_len: source.len(),
        }
    }

    pub(crate) fn tokenize(mut self) -> Vec<Spanned<Token>> {
        let mut tokens = Vec::new();

        // Eat any starting whitespace.
        self.eat_while(|c| matches!(c, ' ' | '\t'));

        loop {
            let start = self.position();

            if let Some(c) = self.bump() {
                let token = match c {
                    '&' | ';' | '|' | '\n' => Token::ControlOp(self.control_op()),
                    '<' | '>' => Token::RedirOp(self.redir_op()),
                    '#' => Token::Comment(self.comment()),
                    _ => todo!(),
                };

                tokens.push(Spanned(
                    token,
                    Span {
                        start,
                        end: self.position(),
                    },
                ));

                // Consume trailing whitespace.
                // TODO: Warn when finding "\r\n"s.
                self.eat_while(|c| matches!(c, ' ' | '\t'));
            } else {
                break;
            }
        }

        tokens
    }

    fn comment(&mut self) -> String {
        self.eat_while(|c| !matches!(c, '\r' | '\n'))
    }

    fn control_op(&mut self) -> ControlOp {
        match self.prev_char {
            '&' => {
                if self.peek_bump(|c| c == '&').is_some() {
                    ControlOp::AndIf
                } else {
                    ControlOp::And
                }
            }
            ';' => {
                if self.peek_bump(|c| c == ';').is_some() {
                    ControlOp::DSemi
                } else {
                    ControlOp::Semi
                }
            }
            '|' => match self.peek_bump(|c| matches!(c, '&' | '|')) {
                Some('&') => ControlOp::OrAnd,
                Some('|') => ControlOp::OrIf,
                _ => ControlOp::Or,
            },
            '\n' => ControlOp::Newline,
            _ => unreachable!("An operator prefix should have been read."),
        }
    }

    fn redir_op(&mut self) -> RedirOp {
        match self.prev_char {
            '<' => match self.peek_bump(|c| matches!(c, '<' | '&' | '>')) {
                Some('<') => match self.peek_bump(|c| matches!(c, '_' | '<')) {
                    Some('-') => RedirOp::DLessDash,
                    Some('<') => RedirOp::TLess,
                    _ => RedirOp::DLess,
                },
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
            _ => unreachable!("An operator prefix should have been read."),
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev_char = c;

        Some(c)
    }

    fn peek_bump(&mut self, condition: impl Fn(char) -> bool) -> Option<char> {
        let c = self.chars.clone().next()?;

        if condition(c) {
            self.bump()
        } else {
            None
        }
    }

    fn eat_while(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut eaten = String::new();

        while let Some(c) = self.peek_bump(&condition) {
            eaten.push(c);
        }

        eaten
    }

    fn position(&self) -> usize {
        self.source_len - self.chars.as_str().len()
    }
}
