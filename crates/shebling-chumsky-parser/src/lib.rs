use chumsky::{prelude::*, util::MaybeRef};
use miette::{NamedSource, Report};
use std::sync::Arc;

// TODO: Document types.

#[derive(Debug, PartialEq)]
struct Spanned<T>(T, SimpleSpan);

#[derive(Debug, PartialEq)]
enum Token {
    Comment(String),
    ControlOp(ControlOp),
    RedirOp(RedirOp),
    Word(Vec<Spanned<WordSgmt>>),
}

#[derive(Debug, PartialEq)]
enum WordSgmt {
    DoubleQuoted(Vec<Spanned<WordSgmt>>),
    Lit(String),
    ParamExpansion(Vec<Spanned<WordSgmt>>),
    SingleQuoted(String),
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

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<LexerError>> {
    fn remove_line_conts(string: &str) -> String {
        string.replace("\\\n", "")
    }

    // Whitespace separating tokens.
    // TODO: Warn when finding "\r\n"s.
    let whitespace = any()
        .filter(|c: &char| matches!(c, ' ' | '\t' | '\n'))
        .repeated();

    // Comments.
    let comment = just('#')
        .then(none_of("\r\n").repeated())
        .map_slice(|comment: &str| Token::Comment(comment.into()));

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

    recursive(|_tokens| {
        let word_sgmt = recursive(|sgmt| {
            // Single quoted strings. Note that we also consider $'' ANSI-C quoting.
            let single_quoted = just('$')
                .or_not()
                .ignore_then(
                    none_of("'")
                        .repeated()
                        .map_slice(|string: &str| WordSgmt::SingleQuoted(string.into()))
                        .padded_by(just("'")),
                )
                .labelled("single quoted string")
                .as_context();

            // Literal word segments.
            let lit_sgmt = |can_escape, special| {
                just('\\')
                    .ignore_then(one_of(can_escape))
                    .or(none_of(special))
                    .repeated()
                    .at_least(1)
                    .collect::<String>()
                    .map(|lit| WordSgmt::Lit(remove_line_conts(&lit)))
            };

            // Unquoted literal characters. Note that when preceded by a backslash, metacharacters
            // become literals.
            let lit = lit_sgmt("|&;<>()$`\\\"' \t\n", "#|&;<>()$`\\\"' \t\n");

            // Double quoted strings. We also consider $"" translated strings.
            // Note that backslashes followed by $, `, ", or \ are removed.
            let double_quoted = just('$')
                .or_not()
                .ignore_then(
                    lit_sgmt("$`\"\\", "$`\"")
                        .map_with_span(Spanned)
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
                    lit_sgmt("}\"$`' \t", "}\"$`' \t")
                        .map_with_span(Spanned)
                        .or(sgmt.and_is(just('}').not()))
                        .padded_by(whitespace)
                        .repeated()
                        .collect()
                        .delimited_by(just('{'), just('}'))
                        .or(text::ident().slice().or(any().slice()).map_with_span(
                            |lit: &str, span| vec![Spanned(WordSgmt::Lit(lit.into()), span)],
                        )),
                )
                .map(WordSgmt::ParamExpansion)
                .labelled("parameter expansion")
                .as_context();

            // let cmd_sub = just('$')
            //     .ignore_then(tokens.delimited_by(just('('), just(')')))
            //     .map(WordSgmt::CmdSub)
            //     .labelled("command substitution")
            //     .as_context();

            choice((single_quoted, double_quoted, param_expansion, lit)).map_with_span(Spanned)
        });

        let word = word_sgmt.repeated().at_least(1).collect().map(Token::Word);

        choice((control_op, redir_op, comment, word))
            .map_with_span(Spanned)
            .padded_by(whitespace)
            .repeated()
            .collect()
    })
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
