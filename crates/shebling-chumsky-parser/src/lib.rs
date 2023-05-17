use chumsky::{prelude::*, util::MaybeRef};
use miette::{NamedSource, Report};
use std::sync::Arc;

#[derive(Debug, PartialEq)]
struct Spanned<'a>(Token<'a>, SimpleSpan);

#[derive(Debug, PartialEq)]
enum Token<'a> {
    ArithExpansion(Vec<Spanned<'a>>),
    CmdSub(Vec<Spanned<'a>>),
    Comment(&'a str),
    ControlOp(ControlOp),
    DoubleQuoted(Vec<Spanned<'a>>),
    Literal(&'a str),
    ParamExpansion(Vec<Spanned<'a>>),
    SingleQuoted(&'a str),
    RedirOp(RedirOp),
    Word(Vec<Spanned<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
enum ControlOp {
    And,
    AndIf,
    DSemi,
    Newline,
    Or,
    OrAnd,
    OrIf,
    Semi,
}

#[derive(Clone, Debug, PartialEq)]
enum RedirOp {
    Clobber,
    DGreat,
    DLess,
    DLessDash,
    Great,
    GreatAnd,
    Less,
    LessAnd,
    LessGreat,
    TLess,
}

#[derive(Debug, PartialEq, thiserror::Error, miette::Diagnostic)]
#[error("invalid {context}!")]
#[diagnostic(code(shebling::lexer::error), severity("error"))]
struct LexerError {
    #[label("{label}")]
    offset: usize,
    label: String,
    context: String,
    // TODO: Help?
}

impl LexerError {
    fn from_expected_found<'a, Iter: IntoIterator<Item = Option<MaybeRef<'a, char>>>>(
        expected: Iter,
        span: SimpleSpan,
    ) -> Self {
        // Just pick the first expected character, and use it for
        // the span's label.
        let expected = expected
            .into_iter()
            .next()
            .map(|c| c.as_deref().copied())
            .flatten();
        let label = if let Some(c) = expected {
            format!("expected '{}'", c)
        } else {
            "unexpected character".into()
        };

        Self {
            offset: span.start,
            label,
            context: "token".into(),
        }
    }
}

impl<'a> chumsky::error::Error<'a, &'a str> for LexerError {
    fn expected_found<Iter: IntoIterator<Item = Option<MaybeRef<'a, char>>>>(
        expected: Iter,
        _found: Option<MaybeRef<'a, char>>,
        span: SimpleSpan,
    ) -> Self {
        Self::from_expected_found(expected, span)
    }

    fn merge_expected_found<Iter: IntoIterator<Item = Option<MaybeRef<'a, char>>>>(
        self,
        expected: Iter,
        _found: Option<MaybeRef<'a, char>>,
        span: SimpleSpan,
    ) -> Self {
        Self::from_expected_found(expected, span)
    }
}

impl<'a> chumsky::label::LabelError<'a, &'a str, &'a str> for LexerError {
    fn label_with(&mut self, label: &'a str) {
        self.label = label.into();
    }

    fn in_context(&mut self, label: &'a str, _span: SimpleSpan) {
        self.context = label.into();
    }
}

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<'a>>, extra::Err<LexerError>> {
    // Comments.
    let comment = just('#')
        .then(none_of("\r\n").repeated())
        .map_slice(Token::Comment);

    // Operators. Note that the order matters here because some
    // operators are prefixes of others.
    let control_op = choice((
        just("&&").to(ControlOp::AndIf),
        just('&').to(ControlOp::And),
        just(";;").to(ControlOp::DSemi),
        just(';').to(ControlOp::Semi),
        just("||").to(ControlOp::OrIf),
        just("|&").to(ControlOp::OrAnd),
        just('|').to(ControlOp::Or),
        just('\n').to(ControlOp::Newline),
    ))
    .map(Token::ControlOp);
    let redir_op = choice((
        just(">|").to(RedirOp::Clobber),
        just(">>").to(RedirOp::DGreat),
        just(">&").to(RedirOp::GreatAnd),
        just('>').to(RedirOp::Great),
        just("<<-").to(RedirOp::DLessDash),
        just("<<<").to(RedirOp::TLess),
        just("<<").to(RedirOp::DLess),
        just("<&").to(RedirOp::LessAnd),
        just("<>").to(RedirOp::LessGreat),
        just('<').to(RedirOp::Less),
    ))
    .map(Token::RedirOp);

    // Double quoted strings. We also consider $"" translated strings.
    let escaped = just('\\').ignore_then(any());
    let double_quoted = just('$')
        .or_not()
        .ignore_then(
            none_of("$`\"\\\n")
                .or(escaped)
                .repeated()
                .at_least(1)
                .slice()
                .map_with_span(|lit, span| Spanned(Token::Literal(lit), span))
                .repeated()
                .collect()
                .padded_by(just('"')),
        )
        .map(Token::DoubleQuoted)
        .labelled("double quoted string")
        .as_context();

    // Single quoted strings. Note that we also consider $'' ANSI-C quoting.
    let single_quoted = just('$')
        .or_not()
        .then(none_of("'").repeated().padded_by(just("'")))
        .map_slice(Token::SingleQuoted)
        .labelled("single quoted string")
        .as_context();

    // Literal characters. Note that when preceded by a backslash, metacharacters
    // become literals.
    let literal = just('\\')
        .ignore_then(one_of("|&;<>()$`\\\"' \t\n"))
        .or(none_of("#|&;<>()$`\\\"' \t\n"))
        .repeated()
        .at_least(1)
        .map_slice(Token::Literal);

    let word = choice((literal, single_quoted, double_quoted))
        .map_with_span(Spanned)
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Word);

    let whitespace = any()
        .filter(|c: &char| matches!(c, ' ' | '\t' | '\n'))
        .repeated();

    choice((control_op, redir_op, comment, word))
        .map_with_span(Spanned)
        .padded_by(whitespace)
        .repeated()
        .collect()
}

pub fn parse(file_path: impl AsRef<str>, source: &str) {
    let (token, errors) = lexer().parse(source).into_output_errors();
    println!("TOKEN: {:#?}", token);
    println!("ERRORS: {:#?}", errors);

    // HACK: When reporting errors, we add a newline to the end of the source
    // so that miette can highlight the last character.
    let source = Arc::new(NamedSource::new(file_path, source.to_owned() + "\n"));

    errors.into_iter().for_each(|err| {
        println!(
            "{:?}",
            Report::new(err).with_source_code(Arc::clone(&source))
        );
    });
}
