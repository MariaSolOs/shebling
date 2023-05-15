use chumsky::prelude::*;
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug)]
enum Token {
    Comment(String),
    Quoted(String),
}

#[derive(Debug, Error, miette::Diagnostic)]
#[error("invalid {context}!")]
#[diagnostic(code(shebling::parser::error), severity("error"))]
struct LexerError {
    #[label("{label}")]
    offset: usize,
    label: String,
    context: String,
    #[help]
    help: Option<String>,
}

impl LexerError {
    fn from_expected_found<
        'a,
        Iter: IntoIterator<Item = Option<chumsky::util::MaybeRef<'a, char>>>,
    >(
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
            offset: span.start(),
            label,
            context: "token".into(),
            help: None,
        }
    }

    // fn set_help() {
    //     todo!()
    // }
}

impl<'a> chumsky::error::Error<'a, &'a str> for LexerError {
    fn expected_found<Iter: IntoIterator<Item = Option<chumsky::util::MaybeRef<'a, char>>>>(
        expected: Iter,
        _found: Option<chumsky::util::MaybeRef<'a, char>>,
        span: SimpleSpan,
    ) -> Self {
        Self::from_expected_found(expected, span)
    }

    fn merge_expected_found<
        Iter: IntoIterator<Item = Option<chumsky::util::MaybeRef<'a, char>>>,
    >(
        self,
        expected: Iter,
        _found: Option<chumsky::util::MaybeRef<'a, char>>,
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

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<(Token, SimpleSpan)>, extra::Err<LexerError>> {
    // Single quoted strings.
    // Also consider $'' ANSI-C quoting.
    let single_quoted = just('$')
        .or_not()
        .then(
            none_of("'")
                .repeated()
                .collect::<String>()
                .padded_by(just("'")),
        )
        .map(|(dollar, string)| {
            let dollar = dollar.map_or("", |_| "$");
            Token::Quoted(format!("{dollar}'{string}'"))
        })
        .labelled("single quoted string")
        .as_context();

    // Double quoted strings.
    // Within double quotes, backslashes that are followed by $, `, ", or \ are removed.
    let escaped = just('\\').ignore_then(one_of("$`\"\\"));
    let double_quoted = none_of('"')
        .and_is(escaped.not())
        .or(escaped)
        .repeated()
        .collect::<String>()
        .padded_by(just('"'))
        .map(|string| {
            // Remove line continuations.
            let string = string.replace("\\\n", "");
            Token::Quoted(format!("\"{string}\""))
        })
        .labelled("double quoted string")
        .as_context();

    // Comments.
    let comment = just('#')
        .ignore_then(none_of("\r\n").repeated().collect::<String>())
        .map(|comment| Token::Comment(format!("#{}", comment)));

    let token = choice((single_quoted, double_quoted, comment));

    token
        .map_with_span(|token, span| (token, span))
        .repeated()
        .collect()
}

pub fn parse(file_path: impl AsRef<str>, source: &str) {
    let (token, errors) = lexer().parse(source).into_output_errors();
    println!("TOKEN: {:#?}", token);
    println!("ERRORS: {:#?}", errors);

    // HACK: When reporting errors, we add a newline to the end of the source
    // so that miette can highlight the last character.
    let source = Arc::new(miette::NamedSource::new(
        file_path,
        source.to_owned() + "\n",
    ));

    errors.into_iter().for_each(|err| {
        println!(
            "{:?}",
            miette::Report::new(err).with_source_code(Arc::clone(&source))
        );
    });
}
