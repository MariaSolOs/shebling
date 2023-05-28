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
            diags: Vec::new(),
        }
    }

    pub(crate) fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub(crate) fn bumped(&mut self, expected: char) -> bool {
        self.peek_bump(|c| c == expected).is_some()
    }

    pub(crate) fn diag(&mut self, kind: ParseDiagnosticKind, label: &'static str) {
        self.diags
            .push(ParseDiagnostic::new(kind, self.position(), label));
    }

    pub(crate) fn eat_while(&mut self, condition: impl Fn(char) -> bool) -> String {
        let mut eaten = String::new();

        while let Some(c) = self.peek_bump(&condition) {
            eaten.push(c);
        }

        eaten
    }

    pub(crate) fn into_remaining_diags(self) -> (&'a str, Vec<ParseDiagnostic>) {
        (self.chars.as_str(), self.diags)
    }

    pub(crate) fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub(crate) fn position(&self) -> usize {
        self.source_len - self.chars.as_str().len()
    }

    fn peek_bump(&mut self, condition: impl Fn(char) -> bool) -> Option<char> {
        let c = self.peek()?;

        if condition(c) {
            self.bump()
        } else {
            None
        }
    }
}
