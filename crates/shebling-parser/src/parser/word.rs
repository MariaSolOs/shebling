use super::*;

fn array(span: Span) -> ParseResult<Array> {
    // Read the opening brace, warning if it looks like a multi-D array.
    let (span, (first_paren, second_paren)) = terminated(
        pair(
            terminated(position, char('(')),
            opt(peek(preceded(char('('), position))),
        ),
        multi_trivia,
    )(span)?;
    if let Some(second_paren) = second_paren {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusValue)
            .range(Range::new(first_paren, second_paren))
            .help("Doing math? Then you're missing the dollar in $((..)), or use '( (' for nested arrays."),
        );
    }

    // Parse the elements, which may be key-value pairs, arrays, or
    // regular words.
    let (span, elems) = many0(delimited(
        not(char(')')),
        alt((
            map(
                separated_pair(
                    many1(subscript),
                    char('='),
                    alt((into(array), into(word), |span| Ok((span, Value::Empty)))),
                ),
                |(key, value)| KeyValue::new(key, value).into(),
            ),
            into(array),
            into(word),
        )),
        multi_trivia,
    ))(span)?;

    // Read the closing brace.
    let (span, _) = context("expected a closing ')' for this array!", char(')'))(span)?;

    Ok((span, Array::new(elems)))
}

pub(super) fn assign(span: Span) -> ParseResult<Assign> {
    // Parse the variable name and any array indices.
    let (span, (ident, subscripts)) = preceded(
        context(
            "don't use $ on the left side of assignments!",
            not(char('$')),
        ),
        pair(context("invalid identifier!", identifier), many0(subscript)),
    )(span)?;

    // Parse the assignment operator.
    let (span, op) = context(
        "expected an assignment operator!",
        alt((token(BinOp::AddEq), token(BinOp::Eq))),
    )(span)?;

    // Check for ==.
    let (span, trailing) = opt(peek(ranged(token(BinOp::Eq))))(span)?;

    // Read any spaces after the operator.
    let (span, ((right_space, space_range), end_of_cmd)) = pair(
        ranged(map(trivia, |trivia| !trivia.is_empty())),
        followed_by(alt((is_a("\r\n;&|)"), eof))),
    )(span)?;

    let (span, value) = if right_space || end_of_cmd {
        // If there's a space after the operator, the value will be an empty string.
        // We don't warn for IFS because of the common 'IFS= read ...' idiom.
        if ident != "IFS" && right_space && !end_of_cmd {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusValue)
                    .label("space causes the value to be empty", space_range)
                    .help(
                        "If you do want a value, remove the space. Else use '' for an empty value.",
                    ),
            );
        }

        (span, Value::Empty)
    } else {
        // Warn about == because the 2nd = will be part of the value.
        if let Some((_, range)) = trailing {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusValue)
                    .label("this = is part of the value", range)
                    .help("Use a single = for assignments, or [ .. ] / [[ .. ]] for comparisons."),
            );
        }

        // Read the assignment value.
        terminated(alt((into(array), into(word))), trivia)(span)?
    };

    Ok((
        span,
        Assign::new(SubscriptedVar::new(ident, subscripts), value, op),
    ))
}

fn bracketed_glob(span: Span) -> ParseResult<Glob> {
    map(
        delimited(
            char('['),
            pair(
                opt(one_of("!^")),
                alt((
                    map(char(']'), |c| vec![c.into()]),
                    many1(alt((
                        // A POSIX character class.
                        recognize_string(delimited(tag("[:"), alpha1, tag(":]"))),
                        // Some other literal sequence.
                        lit_string("]"),
                        // A globby character.
                        map(alt((char('['), one_of(EXTGLOB_PREFIX))), |c| c.into()),
                    ))),
                )),
            ),
            char(']'),
        ),
        |(neg_prefix, mut sgmts)| {
            if let Some(negation) = neg_prefix {
                // Add the negation prefix to the beginning of the glob.
                sgmts.insert(0, negation.into());
            }

            Glob::new(format!("[{}]", sgmts.concat()))
        },
    )(span)
}

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

fn proc_sub(span: Span) -> ParseResult<ProcSub> {
    map(
        delimited(
            tuple((one_of("<>"), char('('), multi_trivia)),
            many0(term),
            pair(multi_trivia, char(')')),
        ),
        ProcSub::new,
    )(span)
}

fn subscript(span: Span) -> ParseResult<String> {
    delimited(
        char('['),
        recognize_string(context(
            "empty subscript!",
            many1(alt((
                word_sgmt_before_pattern("]"),
                into(lit(trivia1)),
                into(lit(recognize_string(is_a(&*format!(
                    "|&;<>() '\t\n\r\u{A0}{}",
                    DOUBLE_ESCAPABLE
                ))))),
            ))),
        )),
        char(']'),
    )(span)
}

pub(super) fn word(span: Span) -> ParseResult<Word> {
    let (span, (word, range)) = ranged(map(
        context("expected a non-empty word!", many1(word_sgmt)),
        Word::new,
    ))(span)?;

    // Check for misplaced keywords.
    if let Some(lit) = word.as_lit() {
        let lit = lit.value();
        if vec![
            Keyword::Do,
            Keyword::Done,
            Keyword::Esac,
            Keyword::Fi,
            Keyword::Then,
        ]
        .into_iter()
        .any(|keyword| keyword.token() == lit)
        {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label(format!("literal '{}'", lit), range)
                    .help("If intended, quote it. Else add a semicolon or new line before it."),
            );
        }
    }

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
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
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
            into(bracketed_glob),
            // Fallback for other glob prefix characters:
            into(lit(one_of("@!+["))),
            dollar_sgmt,
            into(brace_expansion),
            into(backquoted),
            into(proc_sub),
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
    use crate::parser::tests;

    #[test]
    fn test_array() {
        // Values can be key-value pairs, arrays, or words.
        assert_parse!(
            array("( [foo]=(bar) )"),
            Array::new(vec![KeyValue::new(
                vec!["foo".into()],
                Array::new(vec![tests::word("bar").into()]),
            )
            .into()])
        );
        assert_parse!(
            array("( foo bar )"),
            Array::new(vec![tests::word("foo").into(), tests::word("bar").into()])
        );
        assert_parse!(
            array("( (foo) )"),
            Array::new(vec![Array::new(vec![tests::word("foo").into()]).into()])
        );

        // Looks like a multi-dimensional array.
        assert_parse!(
            array("((foo))"),
            Array::new(vec![Array::new(vec![tests::word("foo").into()]).into()]),
            [((1, 1), (1, 3), ParseDiagnosticKind::SusValue)]
        );

        // Missing the closing brace.
        assert_parse!(array("( foo") => Err((1, 6), Notes: [((1, 6), "expected a closing ')'")]));
    }

    #[test]
    fn test_assign() {
        // Legit assignments.
        assert_parse!(
            assign("x+=1"),
            Assign::new(tests::var("x"), tests::word("1"), BinOp::AddEq)
        );
        assert_parse!(
            assign("arr[0]='foo'"),
            Assign::new(
                SubscriptedVar::new("arr", vec!["0".into()]),
                Word::new(vec![SingleQuoted::new("foo").into()]),
                BinOp::Eq,
            )
        );
        assert_parse!(
            assign("foo=(bar baz)"),
            Assign::new(
                tests::var("foo"),
                Array::new(vec![tests::word("bar").into(), tests::word("baz").into()]),
                BinOp::Eq,
            )
        );

        // Warn about empty values if they're not ending the command.
        assert_parse!(
            assign("foo="),
            Assign::new(tests::var("foo"), Value::Empty, BinOp::Eq)
        );
        assert_parse!(
            assign("foo= bar") => "bar",
            Assign::new(tests::var("foo"), Value::Empty, BinOp::Eq),
            [((1, 5), (1, 6), ParseDiagnosticKind::SusValue)]
        );
        // ...but IFS is the exception.
        assert_parse!(
            assign("IFS= "),
            Assign::new(tests::var("IFS"), Value::Empty, BinOp::Eq)
        );

        // Wrong assignment operator.
        assert_parse!(
            assign("foo==bar"),
            Assign::new(tests::var("foo"), tests::word("=bar"), BinOp::Eq),
            [((1, 5), (1, 6), ParseDiagnosticKind::SusValue)]
        );

        // Invalid identifier.
        assert_parse!(assign("1foo=bar") => Err((1, 1), Notes: [((1, 1), "invalid identifier")]));

        // Cannot have a space before the operator.
        assert_parse!(assign("foo =bar") => Err((1, 4), Notes: [((1, 4), "expected an assignment operator")]));

        // Variable names shouldn't be prefixed with a $.
        assert_parse!(assign("$foo=bar") => Err((1, 1), Notes: [((1, 1), "$ on the left side")]));
    }

    #[test]
    fn test_bracketed_glob() {
        // Predefined character class.
        assert_parse!(bracketed_glob("[[:alpha:]]"), Glob::new("[[:alpha:]]"));

        // Can be negated and have multiple segments.
        assert_parse!(
            bracketed_glob("[^[:alpha:]1-9]"),
            Glob::new("[^[:alpha:]1-9]")
        );

        // ']' can be matched if it's the only character in the class.
        assert_parse!(bracketed_glob("[]]"), Glob::new("[]]"));
    }

    #[test]
    fn test_identifier() {
        // Valid identifiers.
        assert_parse!(identifier("foo"), "foo");
        assert_parse!(identifier("_foo0"), "_foo0");

        // Identifiers can't start with a number.
        assert_parse!(identifier("0foo") => Err(1, 1));

        // Cannot be empty.
        assert_parse!(identifier("") => Err(1, 1));
    }

    #[test]
    fn test_lit_string() {
        let lit = lit_string("");

        // Some non-escaped, not special, sequence of characters.
        assert_parse!(lit("foo"), "foo");

        // Escaped literals that generate no warnings.
        assert_parse!(lit("\\$"), "$");
        assert_parse!(lit("\\{"), "{");

        // Warn about commented line continuations.
        assert_parse!(lit("\\\n# foo \\\n") => "\n", "", [((2, 8), ParseDiagnosticKind::BadEscape)]);

        // Warn about trailing space after a line continuation.
        assert_parse!(
            lit("\\  \n"),
            " ",
            [((1, 2), (2, 1), ParseDiagnosticKind::BadSpace)]
        );
        assert_parse!(
            lit("\\  "),
            " ",
            [((1, 2), (1, 4), ParseDiagnosticKind::BadSpace)]
        );

        // "Escaped characters" where the backslash is ignored.
        assert_parse!(
            lit("\\t"),
            "t",
            [((1, 1), (1, 3), ParseDiagnosticKind::BadEscape)]
        );
        assert_parse!(
            lit("\\a"),
            "a",
            [((1, 1), (1, 3), ParseDiagnosticKind::BadEscape)]
        );

        // Cannot be empty.
        assert_parse!(lit("") => Err(1, 1));
        // If not escaped, it cannot be a special character.
        assert_parse!(lit("$") => Err(1, 1));
    }

    #[test]
    fn test_subscript() {
        // There has to be something inside the brackets, even if it's
        // just space.
        assert_parse!(subscript("[0]"), "0");
        assert_parse!(subscript("[ ]"), " ");
        assert_parse!(subscript("[]") => Err((1, 2), Notes: [((1, 2), "empty subscript")]));
    }

    #[test]
    fn test_word() {
        // Can contain multiple segments, as long as there are no metacharacters in between.
        assert_parse!(
            word("'foo'*$bar"),
            Word::new(vec![
                SingleQuoted::new("foo").into(),
                Glob::new("*").into(),
                DollarExp::Var(tests::var("bar")).into()
            ])
        );

        // Warn about literal keywords.
        assert_parse!(
            word("then"),
            tests::word("then"),
            [((1, 1), (1, 5), ParseDiagnosticKind::SusToken)]
        );

        // Can't be empty.
        assert_parse!(word("") => Err((1, 1), Notes: [((1, 1), "expected a non-empty word")]));
    }

    #[test]
    fn test_word_sgmt() {
        // Warn about misplaced parentheses.
        assert_parse!(word_sgmt("(foo)") => Err(
            (1, 1),
            Notes: [((1, 1), "escape this parenthesis")]
        ));

        // Curly brace being parsed as a literal.
        assert_parse!(
            word_sgmt("{foo;}") => "foo;}",
            Lit::new("{").into(),
            [((1, 1), (1, 2), ParseDiagnosticKind::SusToken)]
        );
    }
}
