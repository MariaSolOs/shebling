// TODO: Remove this after development.
#![allow(dead_code)]

use std::{fmt, str::Chars};

// TODO: Document types and function logic.

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
    Word(Vec<Spanned<WordSgmt>>),
}

#[derive(Debug)]
pub(crate) enum WordSgmt {
    DoubleQuoted {
        sgmts: Vec<Spanned<WordSgmt>>,
        closed: bool,
    },
    Lit(String),
    SingleQuoted {
        string: String,
        closed: bool,
    },
}

// TODO: Create an enum with specific errors.
#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("Lexer bailed!")]
#[diagnostic(code(shebling::lexer::error), severity("error"))]
pub(crate) struct LexerError {
    #[label("{}", label.unwrap_or("stopped here"))]
    offset: usize,
    label: Option<&'static str>,
    // TODO: Help?
}

pub(crate) struct Lexer<'a> {
    chars: Chars<'a>,
    source_len: usize,
    errors: Vec<LexerError>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars(),
            source_len: source.len(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn tokenize(mut self) -> (Vec<Spanned<Token>>, Vec<LexerError>) {
        let mut tokens = Vec::new();

        // Eat any starting whitespace.
        self.eat_while(|c| matches!(c, ' ' | '\t'));

        while let Some(c) = self.peek() {
            let start = self.position();

            let token = match c {
                '&' | ';' | '|' | '\n' => self.control_op(),
                '<' | '>' => self.redir_op(),
                '#' => self.comment(),
                _ => self.word(),
            };

            tokens.push(Spanned(token, self.capture_span(start)));

            // Consume trailing whitespace.
            self.eat_while(|c| matches!(c, ' ' | '\t'));
        }

        (tokens, self.errors)
    }

    // region: Individual tokenizers.
    fn comment(&mut self) -> Token {
        assert!(self.bump().is_some_and(|c| c == '#'));

        Token::Comment(self.eat_while(|c| !matches!(c, '\r' | '\n')))
    }

    fn control_op(&mut self) -> Token {
        let op = match self
            .bump()
            .expect("tokenize() should have peeked something")
        {
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
            // TODO: Warn when finding "\r\n"s.
            _ => unreachable!("tokenize() should have peeked an operator prefix."),
        };

        Token::ControlOp(op)
    }

    fn double_quoted(&mut self) -> WordSgmt {
        assert!(self.bump().is_some_and(|c| c == '"'));

        let mut sgmts = Vec::new();

        while let Some(c) = self.peek() {
            let sgmt_start = self.position();

            let sgmt = if let Some(lit) = self.lit("\\\"$`", "$`\"") {
                WordSgmt::Lit(lit)
            } else {
                match c {
                    // Reached the end of the string.
                    '"' => break,
                    _ => todo!(),
                }
            };

            sgmts.push(Spanned(sgmt, self.capture_span(sgmt_start)));
        }

        WordSgmt::DoubleQuoted {
            sgmts,
            closed: if let Some(c) = self.bump() {
                assert!(c == '"');
                true
            } else {
                self.report_error("missing closing double quote");
                false
            },
        }
    }

    fn lit(&mut self, can_escape: &str, stop_with: &str) -> Option<String> {
        let mut lit = String::new();

        while let Some(c) = self.peek_bump(|c| !stop_with.contains(c)) {
            match c {
                '\\' => match self.bump() {
                    Some('\n') => {
                        // Line continuation, don't add anything to the literal.
                    }
                    Some(c) => {
                        if !can_escape.contains(c) {
                            lit.push('\\');
                        }
                        lit.push(c);
                    }
                    None => {
                        self.report_error("no character to escape");
                    }
                },
                c => lit.push(c),
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
            _ => unreachable!("tokenize() should have peeked an operator prefix."),
        };

        Token::RedirOp(op)
    }

    fn single_quoted(&mut self) -> WordSgmt {
        assert!(self.bump().is_some_and(|c| c == '\''));

        let string = self.eat_while(|c| c != '\'');

        WordSgmt::SingleQuoted {
            string,
            closed: if let Some(c) = self.bump() {
                assert!(c == '\'');
                true
            } else {
                self.report_error("missing closing single quote");
                false
            },
        }
    }

    fn word(&mut self) -> Token {
        let mut word = Vec::new();

        while let Some(c) = self.peek() {
            let sgmt_start = self.position();
            let sgmt = match c {
                '"' => self.double_quoted(),
                '\'' => self.single_quoted(),
                '$' => {
                    self.bump();

                    match self.peek() {
                        Some('"') => self.double_quoted(),
                        Some('\'') => self.single_quoted(),
                        _ => todo!(),
                    }
                }
                _ => {
                    if let Some(lit) = self.lit("|&;<>()$`\\\"' \t\n", "#|&;<>()$`\"' \t\n") {
                        WordSgmt::Lit(lit)
                    } else {
                        break;
                    }
                }
            };

            word.push(Spanned(sgmt, self.capture_span(sgmt_start)));
        }

        Token::Word(word)
    }
    // endregion

    // region: Cursor utilities.
    fn bump(&mut self) -> Option<char> {
        self.chars.next()
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
        Span {
            start,
            end: self.position(),
        }
    }
    // endregion

    fn report_error(&mut self, label: &'static str) {
        self.errors.push(LexerError {
            offset: self.position(),
            label: Some(label),
        });
    }
}
