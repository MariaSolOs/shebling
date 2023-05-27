use std::str::Chars;

use crate::diagnostic::{ParseDiagnostic, ParseDiagnosticKind};

#[derive(Debug)]
pub(crate) struct Cursor<'a> {
    chars: Chars<'a>,
    source_len: usize,
    diags: Vec<ParseDiagnostic>,
}

impl<'a> Cursor<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            source_len: source.len(),
            diags: vec![],
        }
    }

    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub(crate) fn bumped(&mut self, expecting: char) -> bool {
        self.peek_bump(|c| c == expecting).is_some()
    }

    pub(crate) fn eat_while(&mut self, condition: impl Fn(char) -> bool) -> String {
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

    pub(crate) fn diag(&mut self, kind: ParseDiagnosticKind, label: &'static str) {
        self.diags
            .push(ParseDiagnostic::new(kind, self.position(), label));
    }
}
