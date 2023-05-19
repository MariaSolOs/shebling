use chumsky::{prelude::*, util::MaybeRef};
use miette::{NamedSource, Report};
use std::sync::Arc;

#[derive(Debug, PartialEq)]
struct Spanned<T>(T, SimpleSpan);

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Comment(&'a str),
    ControlOp(ControlOp),
    RedirOp(RedirOp),
    Word(Vec<Spanned<WordSgmt<'a>>>),
}

#[derive(Debug, PartialEq)]
enum WordSgmt<'a> {
    DoubleQuoted(Vec<Spanned<WordSgmt<'a>>>),
    Lit(&'a str),
    ParamExpansion(Vec<Spanned<WordSgmt<'a>>>),
    SingleQuoted(&'a str),
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

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token<'a>>>, extra::Err<LexerError>> {
    // Whitespace separating tokens.
    // TODO: Warn when finding "\r\n"s.
    let whitespace = any()
        .filter(|c: &char| matches!(c, ' ' | '\t' | '\n'))
        .repeated();

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

    let word_sgmt = recursive(|sgmt| {
        // Single quoted strings. Note that we also consider $'' ANSI-C quoting.
        let single_quoted = just('$')
            .or_not()
            .then(none_of("'").repeated().padded_by(just("'")))
            .map_slice(WordSgmt::SingleQuoted)
            .labelled("single quoted string")
            .as_context();

        // Unquoted literal characters. Note that when preceded by a backslash, metacharacters
        // become literals.
        let lit = just('\\')
            .ignore_then(one_of("|&;<>()$`\\\"' \t\n"))
            .or(none_of("#|&;<>()$`\\\"' \t\n"))
            .repeated()
            .at_least(1)
            .map_slice(WordSgmt::Lit);

        // Inside quotes and expansions, literal segments can vary.
        let special_lit = |special| {
            just('\\')
                .ignore_then(any())
                .or(none_of(special))
                .repeated()
                .at_least(1)
                .map_slice(WordSgmt::Lit)
                .map_with_span(Spanned)
        };

        // Double quoted strings. We also consider $"" translated strings.
        let double_quoted = just('$')
            .or_not()
            .ignore_then(
                special_lit("$`\"\\\n")
                    .or(sgmt.clone())
                    .repeated()
                    .collect()
                    .padded_by(just('"')),
            )
            .map(WordSgmt::DoubleQuoted)
            .labelled("double quoted string")
            .as_context();

        // Parameter expansions.
        let param_expansion = just('$')
            .ignore_then(
                special_lit("}\"$`' \t")
                    .or(sgmt.and_is(just('}').not()))
                    .padded_by(whitespace)
                    .repeated()
                    .collect()
                    .delimited_by(just('{'), just('}'))
                    .or(text::ident()
                        .slice()
                        .or(any().slice())
                        .map_with_span(|lit, span| vec![Spanned(WordSgmt::Lit(lit), span)])),
            )
            .map(WordSgmt::ParamExpansion)
            .labelled("parameter expansion")
            .as_context();

        choice((single_quoted, double_quoted, param_expansion, lit)).map_with_span(Spanned)
    });

    let word = word_sgmt.repeated().at_least(1).collect().map(Token::Word);

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
