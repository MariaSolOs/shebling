use super::*;
use expansion::EXTGLOB_PREFIX;
use quoted::{DOUBLE_ESCAPABLE, DOUBLE_UNIQUOTES, SINGLE_UNIQUOTES};

pub(super) fn identifier(span: Span) -> ParseResult<String> {
    recognize_string(pair(
        // Make sure that the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(span)
}

fn lit_string(end_pattern: &'static str) -> impl Fn(Span) -> ParseResult<String> {
    fn escaped_lit(span: Span) -> ParseResult<String> {
        let (mut span, (start, c)) = preceded(
            backslash,
            pair(
                position,
                alt((
                    line_space,
                    one_of(DOUBLE_ESCAPABLE),
                    one_of(&*format!("|&;<>()'\n\r[]{{}}.,~#{}", EXTGLOB_PREFIX)),
                )),
            ),
        )(span)?;

        if c == ' ' {
            // Backslash followed by whitespace breaks the line break.
            let trailing_space;
            (span, trailing_space) = opt(pair(
                many0(line_space),
                alt((line_ending, value(char::default(), eof))),
            ))(span)?;

            if trailing_space.is_some() {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::BadSpace)
                    .label("trailing space breaks the line continuation", Range::new(start, &span))
                    .help("Remove the spaces to continue the line, or quote it for literal spacing.")
                );
            }
        }

        Ok((span, c.into()))
    }

    fn failed_escaped(span: Span) -> ParseResult<String> {
        // Looks like an escaped character, but isn't.
        let (span, (c, range)) = ranged(preceded(backslash, anychar))(span)?;

        let help = if let Some((name, fix)) = match c {
            'n' => Some(("new line", "a literal quoted new line")),
            't' => Some(("tab", "$(printf 't')")),
            'r' => Some(("carriage return", "(printf 'r')")),
            _ => None,
        } {
            format!("For a {}, use {} instead.", name, fix)
        } else {
            "For a literal backslash, single quote or escape it.".into()
        };

        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                .label("the backslash here will be ignored", range)
                .help(help),
        );

        Ok((span, c.into()))
    }

    move |span| {
        map(
            many1(alt((
                // Special case for line continuation + commented sequences.
                // By calling trivia() after the peek, we make sure that the lint for '# foo \' is reported.
                value(String::new(), terminated(peek(line_continuation), trivia)),
                alt((escaped_lit, failed_escaped)),
                // Some other sequence of non-special, unescaped characters.
                recognize_string(is_not(&*format!(
                    "[{{}}|&;<>() '\t\n\r\u{A0}{}{}{}{}{}",
                    EXTGLOB_PREFIX,
                    DOUBLE_ESCAPABLE,
                    SINGLE_UNIQUOTES,
                    DOUBLE_UNIQUOTES,
                    end_pattern
                ))),
            ))),
            |lits| lits.concat(),
        )(span)
    }
}

pub(super) fn lit_word_sgmt<'a>(
    end_pattern: &'static str,
) -> impl FnMut(Span<'a>) -> ParseResult<WordSgmt> {
    into(lit(lit_string(end_pattern)))
}

pub(super) fn word(span: Span) -> ParseResult<Word> {
    let (span, (word, range)) = ranged(map(
        context("expected a non-empty word!", many1(word_sgmt)),
        Word::new,
    ))(span)?;

    // TODO // Check for misplaced keywords.
    // if let Some(lit) = word.as_lit() {
    //     let lit = lit.value();
    //     if vec![
    //         Keyword::Do,
    //         Keyword::Done,
    //         Keyword::Esac,
    //         Keyword::Fi,
    //         Keyword::Then,
    //     ]
    //     .into_iter()
    //     .any(|keyword| keyword.token() == lit)
    //     {
    //         span.extra.report(Lint::with_message(
    //             range,
    //             LITERAL_KEYWORD,
    //             format_literal_keyword(lit),
    //         ));
    //     }
    // }

    Ok((span, word))
}

pub(super) fn word_sgmt(span: Span) -> ParseResult<WordSgmt> {
    word_sgmt_before_pattern("")(span)
}

fn word_sgmt_before_pattern<'a>(
    pattern: &'static str,
) -> impl FnMut(Span<'a>) -> ParseResult<WordSgmt> {
    fn lit_curly(span: Span) -> ParseResult<Lit> {
        // Curly that's not a keyword.
        let (span, (curly, range)) = ranged(one_of("{}"))(span)?;
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::UnexpectedToken)
                .label("literal curly", range)
                .help("If intended, quote it. Else add a semicolon or new line before it."),
        );

        Ok((span, Lit::new(curly)))
    }

    move |span| {
        // Check that the segment isn't the end pattern.
        let (span, _) = not(one_of(pattern))(span)?;

        // Word segments can't begin with a parenthesis.
        let (span, _) = context("forgot to escape this parenthesis?", not(char('(')))(span)?;

        alt((
            into(single_quoted),
            into(double_quoted),
            into(extglob),
            // Regex match characters:
            map(one_of("*?"), |c| Glob::new(c).into()),
            // TODO into(bracketed_glob),
            // Fallback for other glob prefix characters:
            into(lit(one_of("@!+["))),
            // TODO unquoted_dollar_sgmt,
            // TODO into(brace_expansion),
            // TODO  into(backquoted(false)),
            // TODO into(proc_sub),
            into(lit(alt((single_uniquote, double_uniquote)))),
            lit_word_sgmt(pattern),
            // Literal curly braces:
            into(alt((lit(recognize_string(tag("{}"))), lit_curly))),
        ))(span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        // Valid identifiers.
        assert_parse!(identifier("foo") => "", "foo");
        assert_parse!(identifier("_foo0") => "", "_foo0");

        // Identifiers can't start with a number.
        assert_parse!(identifier("0foo") => Err(1, 1));

        // Cannot be empty.
        assert_parse!(identifier("") => Err(1, 1));
    }

    #[test]
    fn test_lit_string() {
        let lit = lit_string("");

        // Some non-escaped, not special, sequence of characters.
        assert_parse!(lit("foo") => "", "foo");

        // Escaped literals that generate no warnings.
        assert_parse!(lit("\\$") => "", "$");
        assert_parse!(lit("\\{") => "", "{");

        // Warn about commented line continuations.
        assert_parse!(lit("\\\n# foo \\\n") => "\n", "", [((2, 8), ParseDiagnosticKind::BadEscape)]);

        // Warn about trailing space after a line continuation.
        assert_parse!(lit("\\  \n") => "", " ", [((1, 2), (2, 1), ParseDiagnosticKind::BadSpace)]);
        assert_parse!(lit("\\  ") => "", " ", [((1, 2), (1, 4), ParseDiagnosticKind::BadSpace)]);

        // "Escaped characters" where the backslash is ignored.
        assert_parse!(lit("\\t") => "", "t", [((1, 1), (1, 3), ParseDiagnosticKind::BadEscape)]);
        assert_parse!(lit("\\a") => "", "a", [((1, 1), (1, 3), ParseDiagnosticKind::BadEscape)]);

        // Cannot be empty.
        assert_parse!(lit("") => Err(1, 1));
        // If not escaped, it cannot be a special character.
        assert_parse!(lit("$") => Err(1, 1));
    }

    #[test]
    fn test_word() {
        // TODO
    }

    #[test]
    fn test_word_sgmt() {
        // TODO
    }
}
