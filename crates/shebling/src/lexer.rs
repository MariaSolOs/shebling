// TODO: Remove this after development.
#![allow(dead_code)]

use std::{fmt, str::Chars};

// TODO: Document types and function logic.

#[derive(PartialEq)]
struct Span {
    start: usize,
    end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Spanned<T>(T, Span);

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Comment(String),
    ControlOp(ControlOp),
    LParen,
    RedirOp(RedirOp),
    RParen,
    Word(Vec<Spanned<WordSgmt>>),
}

#[derive(Debug, PartialEq)]
pub(crate) enum WordSgmt {
    CmdSub {
        tokens: Vec<Spanned<Token>>,
        closed: bool,
    },
    DoubleQuoted {
        sgmts: Vec<Spanned<WordSgmt>>,
        closed: bool,
    },
    Lit(String),
    ParamExpansion(Vec<Spanned<WordSgmt>>),
    SingleQuoted {
        string: String,
        closed: bool,
    },
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub(crate) enum LexerDiagnostic {
    #[error("CRLF line ending!")]
    #[diagnostic(
        code(shebling::cr_lf),
        help("Try running the script through tr -d '\\r'.")
    )]
    CrLf(#[label("literal carriage return")] usize),

    #[error("unclosed command substitution!")]
    #[diagnostic(code(shebling::unclosed_cmd_sub))]
    UnclosedCmdSub(#[label("missing closing parenthesis")] usize),

    #[error("unclosed {1} string!")]
    #[diagnostic(code(shebling::unclosed_string))]
    UnclosedString(#[label("missing closing quote")] usize, &'static str),
}

pub(crate) struct Lexer<'a> {
    chars: Chars<'a>,
    source_len: usize,
    diags: Vec<LexerDiagnostic>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars(),
            source_len: source.len(),
            diags: Vec::new(),
        }
    }

    pub(crate) fn tokenize(mut self) -> (Vec<Spanned<Token>>, Vec<LexerDiagnostic>) {
        let mut tokens = Vec::new();

        // Eat any starting whitespace.
        self.blanks();

        while let Some(token) = self.token() {
            tokens.push(token);

            // Consume trailing whitespace after each token.
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

    fn cmd_sub(&mut self) -> WordSgmt {
        assert!(self.bump().is_some_and(|c| c == '('));

        let mut tokens = Vec::new();
        while let Some(token) = self.token() {
            if token.0 == Token::RParen {
                return WordSgmt::CmdSub {
                    tokens,
                    closed: true,
                };
            } else {
                tokens.push(token);
                self.blanks();
            }
        }

        // If we get here, we didn't find the closing paren.
        self.diags
            .push(LexerDiagnostic::UnclosedCmdSub(self.position()));

        WordSgmt::CmdSub {
            tokens,
            closed: false,
        }
    }

    fn comment(&mut self) -> Token {
        assert!(self.bump().is_some_and(|c| c == '#'));

        Token::Comment(self.eat_while(|c| c != '\n'))
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
                self.diags.push(LexerDiagnostic::UnclosedString(
                    self.position(),
                    "double quoted",
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
                    Some(c) => {
                        if !can_escape.contains(c) {
                            lit.push('\\');
                        }
                        lit.push(c);
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

    fn param_expansion(&mut self) -> WordSgmt {
        todo!()
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
                self.diags.push(LexerDiagnostic::UnclosedString(
                    self.position(),
                    "single quoted",
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

                    self.control_op()
                }
                '<' | '>' => self.redir_op(),
                '#' => self.comment(),
                '(' => {
                    self.bump();
                    Token::LParen
                }
                ')' => {
                    self.bump();
                    Token::RParen
                }
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

            Some(Spanned(token, self.capture_span(start)))
        } else {
            None
        }
    }

    fn word(&mut self) -> Option<Token> {
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
                        Some('(') => self.cmd_sub(),
                        Some(_) => self.param_expansion(),
                        None => {
                            // This is technically undefined behavior, but we'll just treat it as
                            // a literal dollar.
                            WordSgmt::Lit("$".into())
                        }
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

            word.push(Spanned(sgmt, self.capture_span(sgmt_start)));
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

        Span {
            start,
            end: self.position(),
        }
    }
    // endregion
}
