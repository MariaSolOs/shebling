use once_cell::sync::Lazy;
use std::collections::HashSet;

use super::*;
use crate::location::Location;
use expansion::EXTGLOB_PREFIX;
use quoted::DOUBLE_ESCAPABLE;

const UNIDASHES: &str =
    "\u{058A}\u{05BE}\u{2010}\u{2011}\u{2012}\u{2013}\u{2014}\u{2015}\u{FE63}\u{FF0D}";

// TODO: Use the standard library's once_cell when it's stable.
pub(crate) static COMMON_COMMANDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    // Taken from https://github.com/koalaman/shellcheck/blob/b1ca3929e387446f3e3db023d716cf3787370437/src/ShellCheck/Data.hs#L96.
    HashSet::from_iter([
        "admin",
        "alias",
        "ar",
        "asa",
        "at",
        "awk",
        "basename",
        "batch",
        "bc",
        "bg",
        "break",
        "c99",
        "cal",
        "cat",
        "cd",
        "cflow",
        "chgrp",
        "chmod",
        "chown",
        "cksum",
        "cmp",
        "colon",
        "comm",
        "command",
        "compress",
        "continue",
        "cp",
        "crontab",
        "csplit",
        "ctags",
        "cut",
        "cxref",
        "date",
        "dd",
        "delta",
        "df",
        "diff",
        "dirname",
        "dot",
        "du",
        "echo",
        "ed",
        "env",
        "eval",
        "ex",
        "exec",
        "exit",
        "expand",
        "export",
        "expr",
        "fc",
        "fg",
        "file",
        "find",
        "fold",
        "fort77",
        "fuser",
        "gencat",
        "get",
        "getconf",
        "getopts",
        "grep",
        "hash",
        "head",
        "iconv",
        "ipcrm",
        "ipcs",
        "jobs",
        "join",
        "kill",
        "lex",
        "link",
        "ln",
        "locale",
        "localedef",
        "logger",
        "logname",
        "lp",
        "ls",
        "m4",
        "mailx",
        "make",
        "man",
        "mesg",
        "mkdir",
        "mkfifo",
        "more",
        "mv",
        "newgrp",
        "nice",
        "nl",
        "nm",
        "nohup",
        "od",
        "paste",
        "patch",
        "pathchk",
        "pax",
        "pr",
        "printf",
        "prs",
        "ps",
        "pwd",
        "qalter",
        "qdel",
        "qhold",
        "qmove",
        "qmsg",
        "qrerun",
        "qrls",
        "qselect",
        "qsig",
        "qstat",
        "qsub",
        "read",
        "readonly",
        "renice",
        "return",
        "rm",
        "rmdel",
        "rmdir",
        "sact",
        "sccs",
        "sed",
        "set",
        "sh",
        "shift",
        "sleep",
        "sort",
        "split",
        "strings",
        "strip",
        "stty",
        "tabs",
        "tail",
        "talk",
        "tee",
        "test",
        "time",
        "times",
        "touch",
        "tput",
        "tr",
        "trap",
        "tsort",
        "tty",
        "type",
        "ulimit",
        "umask",
        "unalias",
        "uname",
        "uncompress",
        "unexpand",
        "unget",
        "uniq",
        "unlink",
        "unset",
        "uucp",
        "uudecode",
        "uuencode",
        "uustat",
        "uux",
        "val",
        "vi",
        "wait",
        "wc",
        "what",
        "who",
        "write",
        "xargs",
        "yacc",
        "zcat",
    ])
});

pub(super) fn cond(span: Span) -> ParseResult<Cond> {
    fn and<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        chained(
            move |span| maybe_negated_expr(single_bracketed)(span),
            logic_op(single_bracketed, BinOp::And, "-a"),
        )
    }

    fn bin_expr<'a>(single_bracketed: bool) -> impl Fn(Span<'a>) -> ParseResult<CondBinExpr> {
        move |span| {
            // Read the first argument.
            let (span, left) = cond_word(span)?;

            // Parse the operator.
            let (span, op) = delimited(
                pair(trivia, peek_arith),
                map(
                    escaped_or_quoted(alt((
                        verify(flag_op, |cond_op| cond_op != "-o" && cond_op != "-a"),
                        into(alt((
                            token(BinOp::EqEq),
                            token(BinOp::Ne),
                            token(BinOp::Le),
                            token(BinOp::Ge),
                            token(BinOp::Match),
                            token(BinOp::Gt),
                            token(BinOp::Lt),
                            token(BinOp::Eq),
                        ))),
                    ))),
                    |(op, _)| op,
                ),
                cond_space(true, single_bracketed),
            )(span)?;

            // Read the second argument.
            let (span, right) = terminated(
                cut(context(
                    "expected another argument for the binary expression!",
                    // The 2nd argument is parsed separately if the operator is =~.
                    |span| {
                        if op == BinOp::Match {
                            map(many1(regex_sgmts), |sgmts| {
                                let sgmts = sgmts.into_iter().flatten().collect::<Vec<_>>();
                                Word::new(sgmts).into()
                            })(span)
                        } else {
                            cond_word(span)
                        }
                    },
                )),
                trivia,
            )(span)?;

            Ok((span, CondBinExpr::new(left, right, op)))
        }
    }

    fn bin_or_nullary_expr<'a>(
        single_bracketed: bool,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        move |span| {
            // Warn if the expression looks like a group.
            let (span, bracket) = opt(peek(ranged(char('['))))(span)?;
            if let Some((_, range)) = bracket {
                let (test, group) = if single_bracketed {
                    ("[ .. ]", "\\( .. \\)")
                } else {
                    ("[[ .. ]]", "( .. )")
                };
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::UnexpectedToken)
                        .range(range)
                        .help(format!(
                            "If grouping expressions inside {}, use {}.",
                            test, group
                        )),
                );
            }

            alt((into(bin_expr(single_bracketed)), nullary_expr))(span)
        }
    }

    fn chained<'a, E, O>(expr: E, op: O) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr>
    where
        E: Fn(Span) -> ParseResult<CondExpr> + Copy,
        O: FnMut(Span<'a>) -> ParseResult<CondOp>,
    {
        // Parse >= 1 expressions and then apply the operator in a left associative way.
        map(pair(expr, many0(pair(op, expr))), |(head, tail)| {
            tail.into_iter().fold(head, |acc, (op, expr)| {
                CondBinExpr::new(acc, expr, op).into()
            })
        })
    }

    fn cond_space(required: bool, single_bracketed: bool) -> impl Fn(Span) -> ParseResult<String> {
        move |span| {
            let (span, (space, range)) = ranged(multi_trivia)(span)?;

            if required && space.is_empty() {
                span.extra
                    .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(range));
            }
            if single_bracketed && space.contains('\n') {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingEscape)
                        .label("you need to escape line breaks inside [ .. ]", range),
                );
            }

            Ok((span, space))
        }
    }

    fn cond_word(span: Span) -> ParseResult<Word> {
        // Peek a misplaced bracket.
        let (span, _) = context("unexpected bracket!", not(pair(trivia, char(']'))))(span)?;

        // Parse the word.
        let (span, word) = context("invalid condition argument!", word)(span)?;

        // Check what the last character isn't closing the condition.
        if let WordSgmt::Lit(lit) = word
            .sgmts()
            .last()
            .expect("word() parses at least one segment.")
        {
            if lit.value().ends_with(']')
                && word.sgmts().iter().all(|sgmt| {
                    if let WordSgmt::Lit(lit) = sgmt {
                        !lit.value().contains('[')
                    } else {
                        true
                    }
                })
            {
                // The word doesn't contain a literal [, so it doesn't look like
                // an array or glob. Error!
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                        .range(Location::from(&span).translate(-1)),
                );

                return fail(span);
            }
        }

        Ok((span, word))
    }

    fn content<'a>(single_bracketed: bool) -> impl Fn(Span<'a>) -> ParseResult<CondExpr> {
        move |span| {
            // Check for if [] being used like if ().
            let (span, ident) = opt(peek(terminated(ranged(identifier), trivia1)))(span)?;
            if let Some((ident, range)) = ident {
                if COMMON_COMMANDS.contains(&*ident) {
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::CLikeCode).label(
                            "[ is a command, not a syntax marker for the if statement!",
                            range,
                        ),
                    );
                }
            }

            or(single_bracketed)(span)
        }
    }

    fn escaped_or_quoted<'a, P, R>(mut parser: P) -> impl FnMut(Span<'a>) -> ParseResult<(R, bool)>
    where
        P: FnMut(Span<'a>) -> ParseResult<R>,
    {
        move |span| {
            let (mut span, bs) = opt(backslash)(span)?;

            // If there's no backslash, check for quotes.
            let mut quote = None;
            if bs.is_none() {
                (span, quote) = opt(one_of("'\""))(span)?;
            }

            let (mut span, res) = parser(span)?;

            // Verify the closing quote.
            if let Some(quote) = quote {
                (span, _) = char(quote)(span)?
            }

            Ok((span, (res, bs.is_some() || quote.is_some())))
        }
    }

    fn flag_op(span: Span) -> ParseResult<CondOp> {
        // Read the dash, warning if it's a unidash.
        let (span, _) = alt((char('-'), |span| {
            let (span, (unidash, range)) = ranged(one_of(UNIDASHES))(span)?;
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::Unichar).label("unicode dash", range),
            );

            Ok((span, unidash))
        }))(span)?;

        // Parse the flag.
        let (span, flag) = cut(context("expected a test operator!", alpha1))(span)?;

        Ok((span, CondOp::new(format!("-{}", flag))))
    }

    fn group<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondGroup> {
        let paren = |p| {
            move |span| {
                let (span, ((_, escaped), range)) = terminated(
                    ranged(escaped_or_quoted(char(p))),
                    cond_space(single_bracketed, single_bracketed),
                )(span)?;
                // Parentheses should be escaped inside [ .. ].
                if single_bracketed && !escaped {
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::MissingEscape)
                            .range(range)
                            .help("You should use \\( and \\) inside [ .. ], or combine [ .. ] expressions."),
                    )
                }

                // Parentheses should not be escaped inside [[ .. ]].
                if !single_bracketed && escaped {
                    span.extra.diag(
                        ParseDiagnostic::builder(ParseDiagnosticKind::BadEscape)
                            .label(format!("no need to escape {} inside [[ .. ]].", p), range),
                    );
                }

                Ok((span, ()))
            }
        };

        delimited(
            paren('('),
            map(or(single_bracketed), CondGroup::new),
            paren(')'),
        )
    }

    fn logic_op<'a, O: ParseToken>(
        single_bracketed: bool,
        unflagged_op: O,
        flagged_op: &'static str,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondOp> {
        alt((
            // Unflagged logic operator (e.g. &&).
            terminated(
                map(token(unflagged_op), |op| CondOp::new(op.token())),
                cond_space(false, single_bracketed),
            ),
            // Flagged logic operator (e.g. -a).
            terminated(
                verify(flag_op, move |op| op == flagged_op),
                cond_space(true, single_bracketed),
            ),
        ))
    }

    fn maybe_negated_expr<'a>(
        single_bracketed: bool,
    ) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        let expr = move |span| {
            alt((
                into(group(single_bracketed)),
                into(unary_expr(single_bracketed)),
                bin_or_nullary_expr(single_bracketed),
            ))(span)
        };

        // Read an expression, which could be negated.
        terminated(
            alt((
                map(
                    separated_pair(token(UnOp::Not), cond_space(true, single_bracketed), expr),
                    |(op, expr)| CondUnExpr::new(expr, op).into(),
                ),
                expr,
            )),
            cond_space(false, single_bracketed),
        )
    }

    fn nullary_expr(span: Span) -> ParseResult<CondExpr> {
        // Parse the expression.
        let (span, (expr, expr_end)) =
            terminated(pair(cond_word, position), pair(trivia, peek_arith))(span)?;

        // Check if the expression ends with what looks like a binary operator. If
        // so, the user might have forgotten a space.
        if let Some(WordSgmt::Lit(lit)) = expr.sgmts().last() {
            let lit = lit.value();
            // TODO: create a shared public array with these?
            if let Some(op) = vec![
                "-nt", "-ot", "-ef", "-eq", "-ne", "-lt", "-le", "-gt", "-ge", "==", "!=", "=~",
                ">", "<", "<=", ">=", "=",
            ]
            .into_iter()
            .find_map(|op| {
                // Consider both escaped and unescaped operators.
                let escaped = format!("\\{}", op);
                if lit.ends_with(&escaped) {
                    Some(escaped)
                } else if lit.ends_with(op) {
                    Some(op.into())
                } else {
                    None
                }
            }) {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(
                        Location::from(expr_end).translate((op.len() as isize).wrapping_neg()),
                    ),
                );
            }
        }

        Ok((span, expr.into()))
    }

    fn or<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        chained(
            move |span| and(single_bracketed)(span),
            preceded(peek_arith, logic_op(single_bracketed, BinOp::Or, "-o")),
        )
    }

    fn peek_arith(span: Span) -> ParseResult<()> {
        // Peek a mathy operator character.
        let (span, math_following) = opt(peek(pair(
            position,
            alt((
                preceded(one_of("+*/%"), position),
                delimited(char('-'), position, char(' ')),
            )),
        )))(span)?;

        // Tests should do math stuff inside $((..)).
        if let Some((start, end)) = math_following {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::UnexpectedToken)
                    .label("math operator!", Range::new(start, end))
                    .help("Inside conditions, do math stuff inside $(( .. ))."),
            )
        }

        Ok((span, ()))
    }

    fn regex_sgmts(span: Span) -> ParseResult<Vec<WordSgmt>> {
        alt((
            // Read a group.
            map(
                tuple((
                    into(lit(char('('))),
                    many0(alt((
                        regex_sgmts,
                        map(
                            many1(alt((
                                escaped(""),
                                recognize_string(is_not(&*format!("'(){}", DOUBLE_ESCAPABLE))),
                            ))),
                            |lits| vec![Lit::new(lits.concat()).into()],
                        ),
                    ))),
                    into(lit(char(')'))),
                )),
                |(left, sgmts, right)| {
                    let mut sgmts = sgmts.into_iter().flatten().collect::<Vec<_>>();
                    // Push the group's parens.
                    sgmts.insert(0, left);
                    sgmts.push(right);

                    sgmts
                },
            ),
            // Ungrouped regex segment.
            map(
                alt((
                    into(single_quoted),
                    into(double_quoted),
                    into(dollar_exp),
                    lit_word_sgmt("( "),
                    into(lit(one_of(&*format!("{}{{}}[]|$", EXTGLOB_PREFIX)))),
                )),
                |sgmt| vec![sgmt],
            ),
        ))(span)
    }

    fn unary_expr<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondUnExpr> {
        map(
            separated_pair(
                flag_op,
                cond_space(true, single_bracketed),
                cut(context(
                    "expected an operand for the unary expression!",
                    terminated(cond_word, trivia),
                )),
            ),
            |(op, expr)| CondUnExpr::new(expr, op),
        )
    }

    // Parse the opening bracket.
    let (span, second_bracket) = preceded(char('['), opt(char('[')))(span)?;
    let single_bracketed = second_bracket.is_none();

    // Check spacing.
    let (span, space) = cond_space(true, single_bracketed)(span)?;

    // Get the content (if it isn't an empty condition).
    let (span, bracket_following) = followed_by(char(']'))(span)?;
    let (span, cond) = if bracket_following && !space.is_empty() {
        (span, Cond::new(single_bracketed))
    } else {
        map(content(single_bracketed), |expr| {
            Cond::with_expr(single_bracketed, expr)
        })(span)?
    };

    // Verify the closing bracket.
    let (span, (second_bracket, bracket_range)) = terminated(
        context(
            "expected the test to end here (don't wrap commands in square brackets!)",
            ranged(preceded(char(']'), opt(char(']')))),
        ),
        trivia,
    )(span)?;

    // Make sure the brackets match.
    if (single_bracketed && second_bracket.is_some())
        || (!single_bracketed && second_bracket.is_none())
    {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::UnexpectedToken).label(
                "closing bracket doesn't match the opening one",
                bracket_range,
            ),
        );
    }

    Ok((span, cond))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_cond() {
        // Operators can be escaped or quoted.
        assert_parse!(
            cond("[ foo \\< bar ]") => "",
            Cond::with_expr(
                true,
                CondBinExpr::new(tests::lit_word("foo"), tests::lit_word("bar"), BinOp::Lt),
            )
        );
        assert_parse!(
            cond("[ foo '<' bar ]") => "",
            Cond::with_expr(
                true,
                CondBinExpr::new(tests::lit_word("foo"), tests::lit_word("bar"), BinOp::Lt),
            )
        );

        // Same with parentheses.
        assert_parse!(
            cond("[ '(' foo \\) ]") => "",
            Cond::with_expr(true, CondGroup::new(tests::lit_word("foo")))
        );
        // ...In fact, you have to do it inside single brackets.
        assert_parse!(
            cond("[ '(' foo ) ]") => "",
            Cond::with_expr(true, CondGroup::new(tests::lit_word("foo"))),
            [((1, 11), (1, 12), ParseDiagnosticKind::MissingEscape)]
        );
        // ...But in double brackets is wrong to do it.
        assert_parse!(
            cond("[[ '(' foo ) ]]") => "",
            Cond::with_expr(false, CondGroup::new(tests::lit_word("foo"))),
            [((1, 4), (1, 7), ParseDiagnosticKind::BadEscape)]
        );

        // New lines have to be escaped inside single brackets.
        assert_parse!(cond("[\n]") => "", Cond::new(true), [((1, 2), (2, 1), ParseDiagnosticKind::MissingEscape)]);

        // Empty conditions are legal.
        assert_parse!(cond("[[ ]]") => "", Cond::new(false));

        // Bracket mismatch.
        assert_parse!(
            cond("[ ]]") => "",
            Cond::new(true),
            [((1, 3), (1, 5), ParseDiagnosticKind::UnexpectedToken)]
        );

        // if [ grep foo file ] pitfall.
        assert_parse!(cond("[ grep foo file ]") => Err(
            (1, 8),
            Notes: [((1, 8), "expected the test to end")],
            Diags: [((1, 3), (1, 7), ParseDiagnosticKind::CLikeCode)]
        ));

        // Incorrect opening bracket.
        assert_parse!(cond("]") => Err(1, 1));

        // Missing the closing bracket.
        assert_parse!(cond("[ ") => Err(
            (1, 3),
            Notes: [((1, 3), "expected a non-empty word"), ((1, 3), "invalid condition argument")]
        ));

        // Missing space after the opening bracket.
        assert_parse!(cond("[]") => Err(
            (1, 2),
            Notes: [((1, 2), "unexpected bracket")],
            Diags: [((1, 2), ParseDiagnosticKind::MissingSpace)]
        ));

        // Should use parens for grouping instead of [].
        assert_parse!(cond("[ [ foo ] ]") => Err(
            (1, 5),
            Notes: [((1, 5), "expected the test to end")],
            Diags: [((1, 3), (1, 4), ParseDiagnosticKind::UnexpectedToken)]
        ));

        // Shouldn't use math operators inside test conditions.
        assert_parse!(cond("[ x + 1 ]") => Err(
            (1, 5),
            Notes: [((1, 5), "expected the test to end")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::UnexpectedToken)]
        ));
        assert_parse!(cond("[ x - 1 ]") => Err(
            (1, 6),
            Notes: [((1, 6), "expected a test operator")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::UnexpectedToken)]
        ));

        // Missing spaces around the operator.
        assert_parse!(cond("[ foo= bar ]") => Err(
            (1, 8),
            Notes: [((1, 8), "expected the test to end")],
            Diags: [((1, 6), ParseDiagnosticKind::MissingSpace)]
        ));
        assert_parse!(cond("[ foo\\>= bar ]") => Err(
            (1, 10),
            Notes: [((1, 10), "expected the test to end")],
            Diags: [((1, 7), ParseDiagnosticKind::MissingSpace)]
        ));

        // Word that prematurely ends the condition.
        assert_parse!(cond("[ foo] ]") => Err(
            (1, 7),
            Diags: [((1, 6), ParseDiagnosticKind::MissingSpace)]
        ));
    }
}
