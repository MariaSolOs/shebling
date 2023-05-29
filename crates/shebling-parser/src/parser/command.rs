use once_cell::sync::Lazy;
use std::collections::HashSet;

use super::*;
use crate::{location::Location, ParseContext};

const UNIDASHES: &str =
    "\u{058A}\u{05BE}\u{2010}\u{2011}\u{2012}\u{2013}\u{2014}\u{2015}\u{FE63}\u{FF0D}";

// TODO: Use the standard library's once_cell when it's stable.
static COMMON_COMMANDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
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

pub(super) fn brace_group(span: Span) -> ParseResult<Term> {
    // Check spacing after the opening brace, but note that '{(' is legal.
    let (span, (start, space, paren)) = tuple((
        terminated(position, char('{')),
        multi_trivia,
        followed_by(char('(')),
    ))(span)?;
    if space.is_empty() && !paren {
        span.extra
            .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(&span));
    }

    // Check for empty groups.
    let (span, closing_brace) = opt(peek(preceded(char('}'), position)))(span)?;
    if let Some(end) = closing_brace {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("empty code block", Range::new(start, end)),
        );
    }

    // Parse the braced term.
    let (span, term) = term(span)?;

    // Check the closing brace.
    let (span, _) = terminated(
        context(
            "missing the closing brace (or the semicolon or newline before it)",
            char('}'),
        ),
        trivia,
    )(span)?;

    Ok((span, term))
}

fn case_clause(span: Span) -> ParseResult<CaseClause> {
    fn pattern_word(span: Span) -> ParseResult<Word> {
        let (span, (word, range)) = ranged(map(many1(word_sgmt), Word::new))(span)?;

        // Any literal except 'esac' is valid pattern in a case statement.
        let esac = Keyword::Esac.token();
        if word.as_lit().is_some_and(|lit| lit == esac) {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("literal 'esac'", range)
                    .help("If intended, quote it. Else add a semicolon or new line before it."),
            );
        }

        Ok((span, word))
    }

    fn sep(span: Span) -> ParseResult<Option<ClauseSep>> {
        alt((
            map(
                alt((
                    token(ClauseSep::Continue),
                    token(ClauseSep::Fallthrough),
                    token(ClauseSep::Break),
                )),
                Some,
            ),
            // The separator of the last clause can be omitted.
            map(peek(pair(linebreak, token(Keyword::Esac))), |_| None),
        ))(span)
    }

    // Stop if we peek an 'esac'.
    let (span, _) = not(peek(token(Keyword::Esac)))(span)?;

    // Optional parenthesis:
    let (span, _) = pair(opt(char('(')), trivia)(span)?;

    // Parse the pattern.
    let (span, pattern) = separated_list1(
        terminated(char('|'), trivia),
        terminated(pattern_word, trivia),
    )(span)?;

    // Check the closing parenthesis.
    let (span, _) = pair(
        cut(context("expected a closing ')'!", char(')'))),
        linebreak,
    )(span)?;

    // Parse the clause's body.
    let (span, cmd) = alt((map(peek(sep), |_| None), map(term, Some)))(span)?;

    // Read the separator.
    let (span, sep) = delimited(
        cut(context(
            "did you forget the separator in the previous clause?",
            not(char(')')),
        )),
        sep,
        pair(trivia, linebreak),
    )(span)?;

    Ok((span, CaseClause::new(pattern, cmd, sep)))
}

fn case_cmd(span: Span) -> ParseResult<CaseCmd> {
    terminated(
        map(
            pair(
                // The header:
                delimited(
                    pair(token(Keyword::Case), trivia),
                    word,
                    tuple((
                        multi_trivia,
                        cut(context("expected 'in'!", token(Keyword::In))),
                        trivia,
                        linebreak,
                    )),
                ),
                // The clauses:
                many0(case_clause),
            ),
            |(word, clauses)| CaseCmd::new(word, clauses),
        ),
        cut(context("expected a closing 'esac'!", token(Keyword::Esac))),
    )(span)
}

fn cmd(span: Span) -> ParseResult<Cmd> {
    alt((
        into(compound_cmd),
        into(cond_cmd),
        into(coproc),
        into(simple_cmd),
    ))(span)
}

fn compound_cmd(span: Span) -> ParseResult<CompoundCmd> {
    fn ambiguous_parens(span: Span) -> ParseResult<Construct> {
        // (( starts either an arithmetic expression or opens a subshell.
        let (span, (_, paren_range)) = peek(ranged(tag("((")))(span)?;

        // Try parsing an arithmetic expression. All good if it succeeds.
        let (span, seq) = opt(delimited(tag("(("), arith_seq, tag("))")))(span)?;
        if let Some(seq) = seq {
            return Ok((span, seq.into()));
        }

        // Else try parsing a subshell. If it succeeds, the prefix is ambiguous.
        let (span, subshell) = opt(subshell)(span)?;
        if let Some(subshell) = subshell {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::Ambiguous)
                    .label("ambiguous parens", paren_range)
                    .help("If you want a subshell, space the parentheses."),
            );

            return Ok((span, Construct::Subshell(subshell)));
        }

        cut(context(
            "invalid arithmetic expression!",
            into(delimited(tag("(("), arith_seq, tag("))"))),
        ))(span)
    }

    // Read the command and suffix redirections.
    let (span, (cmd, redirs)) = pair(
        alt((
            map(brace_group, Construct::BraceGroup),
            ambiguous_parens,
            map(subshell, Construct::Subshell),
            map(cond_block(Keyword::While), Construct::While),
            map(cond_block(Keyword::Until), Construct::Until),
            map(if_cmd, Construct::If),
            map(for_loop, Construct::ForLoop),
            preceded(token(Keyword::Select), map(in_listed, Construct::Select)),
            map(case_cmd, Construct::Case),
            map(bats_test, Construct::BatsTest),
            map(function, Construct::Function),
        )),
        many0(redir),
    )(span)?;

    // Check for post-command gibberish.
    let (span, trailing) = opt(peek(preceded(
        not(alt((peek_sus_token, swallow(peek(char('{')))))),
        ranged(many1(word)),
    )))(span)?;
    if let Some((_, range)) = trailing {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("trailing words after compound command", range)
                .help("You might have bad redirections or a missing operator."),
        );
    }

    Ok((span, CompoundCmd::new(cmd, redirs)))
}

fn cond(span: Span) -> ParseResult<Cond> {
    fn and<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        chained(
            move |span| maybe_negated_expr(single_bracketed)(span),
            logic_op(single_bracketed, BinOp::And, "-a"),
        )
    }

    fn bin_expr<'a>(single_bracketed: bool) -> impl Fn(Span<'a>) -> ParseResult<CondExpr> {
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
                                Word::new(sgmts)
                            })(span)
                        } else {
                            cond_word(span)
                        }
                    },
                )),
                trivia,
            )(span)?;

            Ok((span, BinExpr::new(left, right, op).into()))
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
                    ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
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
            tail.into_iter()
                .fold(head, |acc, (op, expr)| BinExpr::new(acc, expr, op).into())
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
            if lit.ends_with(']')
                && word.sgmts().iter().all(|sgmt| {
                    if let WordSgmt::Lit(lit) = sgmt {
                        !lit.contains('[')
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
                        ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode).label(
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

    fn group<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
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
            map(or(single_bracketed), |group| {
                CondExpr::Group(Box::new(group))
            }),
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
                group(single_bracketed),
                unary_expr(single_bracketed),
                bin_or_nullary_expr(single_bracketed),
            ))(span)
        };

        // Read an expression, which could be negated.
        terminated(
            alt((
                map(
                    separated_pair(token(UnOp::Not), cond_space(true, single_bracketed), expr),
                    |(op, expr)| UnExpr::new(expr, op).into(),
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
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
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
                    map(char('('), |c| WordSgmt::Lit(c.into())),
                    many0(alt((
                        regex_sgmts,
                        map(
                            many1(alt((
                                escaped(""),
                                recognize_string(is_not(&*format!("'(){}", DOUBLE_ESCAPABLE))),
                            ))),
                            |lits| vec![WordSgmt::Lit(lits.concat())],
                        ),
                    ))),
                    map(char(')'), |c| WordSgmt::Lit(c.into())),
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
                    map(single_quoted, WordSgmt::SingleQuoted),
                    into(double_quoted),
                    into(dollar_exp),
                    lit_word_sgmt("( "),
                    map(one_of(&*format!("{}{{}}[]|$", EXTGLOB_PREFIX)), |c| {
                        WordSgmt::Lit(c.into())
                    }),
                )),
                |sgmt| vec![sgmt],
            ),
        ))(span)
    }

    fn unary_expr<'a>(single_bracketed: bool) -> impl FnMut(Span<'a>) -> ParseResult<CondExpr> {
        map(
            separated_pair(
                flag_op,
                cond_space(true, single_bracketed),
                cut(context(
                    "expected an operand for the unary expression!",
                    terminated(cond_word, trivia),
                )),
            ),
            |(op, expr)| UnExpr::new(expr, op).into(),
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
        (span, Cond::new(single_bracketed, None))
    } else {
        map(content(single_bracketed), |expr| {
            Cond::new(single_bracketed, Some(expr))
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
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken).label(
                "closing bracket doesn't match the opening one",
                bracket_range,
            ),
        );
    }

    Ok((span, cond))
}

fn cond_block<'a>(keyword: Keyword) -> impl FnMut(Span<'a>) -> ParseResult<CondBlock> {
    preceded(
        pair(token(keyword), trivia),
        map(pair(term, do_group), |(cond, block)| {
            CondBlock::new(cond, block)
        }),
    )
}

fn cond_cmd(span: Span) -> ParseResult<CompoundCmd> {
    // Read the condition and suffix redirections.
    let (span, (cond, redirs)) = pair(cond, many0(redir))(span)?;

    // || or && should be used between test conditions.
    let (span, cond_op) = opt(peek(ranged(consumed(alt((
        value(ControlOp::AndIf, alt((tag("-a"), tag("and")))),
        value(ControlOp::OrIf, alt((tag("-o"), tag("or")))),
    ))))))(span)?;
    if let Some(((bad_op, good_op), range)) = &cond_op {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator)
                .range(*range)
                .help(format!(
                    "Use {} instead of {} between test commands.",
                    good_op.token(),
                    bad_op
                )),
        );
    }

    // Check if there's a trailing word that's not keyword/operator-like.
    let (span, (sus_token, next_word)) = pair(opt(peek_sus_token), opt(peek(ranged(word))))(span)?;
    if let Some((_, range)) = next_word {
        if sus_token.is_none() && cond_op.is_none() {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("unexpected parameter after condition", range)
                    .help("You might be missing a && or ||."),
            )
        }
    }

    Ok((span, CompoundCmd::new(cond, redirs)))
}

fn coproc(span: Span) -> ParseResult<Coproc> {
    preceded(
        pair(token(Keyword::Coproc), whitespace),
        alt((
            map(
                pair(opt(terminated(identifier, whitespace)), compound_cmd),
                |(name, cmd)| Coproc::new(name, cmd),
            ),
            map(simple_cmd, |cmd| Coproc::new(None, cmd)),
        )),
    )(span)
}

fn do_group(span: Span) -> ParseResult<Term> {
    // 'do' should come before the 'done'.
    let (span, _) = context(
        "did you forget the 'do' for this loop?",
        not(token(Keyword::Done)),
    )(span)?;

    // Parse the 'do'.
    let (span, _) = terminated(context("expected 'do'!", token(Keyword::Do)), trivia)(span)?;

    // Warn about 'do;'s.
    let (span, semi) = terminated(opt(ranged(token(ControlOp::Semi))), multi_trivia)(span)?;
    if let Some((_, range)) = semi {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator)
                .label("semicolon after 'do'", range)
                .help("Just delete it."),
        );
    }

    // Parse the term, bailing if the clause is empty.
    let (span, term) = preceded(
        context("empty 'do' clause!", not(token(Keyword::Done))),
        term,
    )(span)?;

    // Parse the 'done'.
    let (span, _) = terminated(context("expected 'done'!", token(Keyword::Done)), trivia)(span)?;

    // If we peek the beginning of a process substitution, the user might have missed a '<'.
    let (span, proc_sub_start) = opt(peek(ranged(tag("<("))))(span)?;
    if let Some((_, range)) = proc_sub_start {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                .label("process substitution?", range)
                .help("For redirection, use 'done < <(cmd)' (you're missing one '<')."),
        );
    }

    Ok((span, term))
}

fn for_loop(span: Span) -> ParseResult<ForLoop> {
    fn arith_for_loop(span: Span) -> ParseResult<ArithForLoop> {
        // Parse the loop's header.
        let (span, header) = delimited(
            arith_parens(
                '(',
                "missing the second ( opening this arithmetic for-loop!",
            ),
            tuple((
                terminated(arith_seq, pair(char(';'), trivia)),
                terminated(arith_seq, pair(char(';'), trivia)),
                terminated(arith_seq, trivia),
            )),
            arith_parens(
                ')',
                "missing the second ) closing this arithmetic for-loop!",
            ),
        )(span)?;

        // There can be an optional separator before the 'do'.
        let (span, _) = delimited(
            trivia,
            opt(alt((
                preceded(token(ControlOp::Semi), linebreak),
                newline_list,
            ))),
            trivia,
        )(span)?;

        // Parse the body.
        let (span, body) = alt((brace_group, do_group))(span)?;

        Ok((span, ArithForLoop::new(header, body)))
    }

    fn arith_parens(paren: char, ctx_msg: &'static str) -> impl Fn(Span) -> ParseResult<()> {
        move |span| {
            // Parse both parentheses.
            let (span, trivia) = delimited(
                char(paren),
                opt(ranged(trivia1)),
                context(ctx_msg, char(paren)),
            )(span)?;

            // Can't have space in-between.
            if let Some((_, range)) = trivia {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::BadSpace)
                        .range(range)
                        .help(format!(
                            "Remove spacing between '{0}{0}' in this arithmetic for-loop.",
                            paren
                        )),
                );
            }

            Ok((span, ()))
        }
    }

    preceded(
        pair(token(Keyword::For), trivia),
        alt((into(arith_for_loop), into(in_listed))),
    )(span)
}

fn if_cmd(span: Span) -> ParseResult<IfCmd> {
    fn branch_term<'a>(ctx: &'static str) -> impl FnMut(Span<'a>) -> ParseResult<Term> {
        // Parse the branch's term, bailing if it's empty.
        alt((
            term,
            cut(preceded(
                peek(alt((
                    token(Keyword::Fi),
                    token(Keyword::Elif),
                    token(Keyword::Else),
                ))),
                context(ctx, fail),
            )),
        ))
    }

    fn else_branch(span: Span) -> ParseResult<Term> {
        // 'else if' is not the same as 'elif'.
        let (span, (start, if_end)) = pair(
            position,
            preceded(
                pair(token(Keyword::Else), trivia),
                opt(peek(preceded(token(Keyword::If), position))),
            ),
        )(span)?;
        if let Some(end) = if_end {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                    .range(Range::new(start, end))
                    .help("Use 'elif' to start another branch."),
            );
        }

        // Warn about 'else;'s.
        let (span, _) = semicolon_check(Keyword::Else)(span)?;

        // Parse the branch's term.
        branch_term("empty 'else' clause!")(span)
    }

    fn if_block<'a>(keyword: Keyword) -> impl FnMut(Span<'a>) -> ParseResult<CondBlock> {
        map(
            pair(
                // Read the keyword, the condition, and the 'then'.
                delimited(
                    pair(token(keyword), multi_trivia),
                    term,
                    tuple((
                        context("expected 'then'!", token(Keyword::Then)),
                        trivia,
                        semicolon_check(Keyword::Then),
                    )),
                ),
                branch_term("empty 'then' clause!"),
            ),
            |(cond, block)| CondBlock::new(cond, block),
        )
    }

    fn semicolon_check(keyword: Keyword) -> impl Fn(Span) -> ParseResult<()> {
        move |span| {
            let (span, semi) = terminated(opt(ranged(token(ControlOp::Semi))), multi_trivia)(span)?;
            if let Some((_, range)) = semi {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::BadOperator)
                        .label(format!("semicolon after '{}'", keyword.token()), range)
                        .help("Just delete it."),
                );
            }

            Ok((span, ()))
        }
    }

    map(
        tuple((
            if_block(Keyword::If),
            many0(if_block(Keyword::Elif)),
            terminated(
                opt(else_branch),
                pair(context("expected 'fi'!", token(Keyword::Fi)), trivia),
            ),
        )),
        |(if_branch, elif_branches, else_term)| IfCmd::new(if_branch, elif_branches, else_term),
    )(span)
}

fn in_list(span: Span) -> ParseResult<Vec<Word>> {
    // Parse the 'in' keyword.
    let (span, _) = pair(token(Keyword::In), trivia)(span)?;

    // Read the list and stop when we hit a 'do', new line or semicolon.
    let (span, (list, _)) = many_till(
        terminated(word, trivia),
        alt((
            |span| {
                let (span, _) = peek(token(Keyword::Do))(span)?;
                // 'do' should be on a new line or statement:
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                        .range(&span)
                        .help("Add a new line or semicolon before 'do'."),
                );

                Ok((span, ()))
            },
            terminated(
                alt((swallow(token(ControlOp::Semi)), swallow(line_ending))),
                multi_trivia,
            ),
        )),
    )(span)?;

    Ok((span, list))
}

fn in_listed(span: Span) -> ParseResult<InListed> {
    // Parse the iterator variable.
    let (span, ((dollar, name), range)) =
        terminated(ranged(pair(opt(char('$')), identifier)), multi_trivia)(span)?;

    // Emit lint if there's a dollar (the iterator variable should be an
    // identifier, not a value).
    if dollar.is_some() {
        span.extra.diag(
            ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                .label("dollar in iterator variable", range)
                .help(format!("Simply use '{}'.", name)),
        );
    }

    // Parse the iterating list.
    let (span, list) = alt((
        in_list,
        map(
            opt(alt((
                preceded(pair(token(ControlOp::Semi), trivia), linebreak),
                newline_list,
            ))),
            |_| vec![],
        ),
    ))(span)?;

    // Parse the body.
    let (span, body) = alt((brace_group, do_group))(span)?;

    Ok((span, InListed::new(name, list, body)))
}

fn peek_sus_token(span: Span) -> ParseResult<()> {
    // Tokens that are often misplaced in compound commands.
    alt((
        swallow(peek(alt((
            token(Keyword::Do),
            token(Keyword::Done),
            token(Keyword::Else),
            token(Keyword::Esac),
            token(Keyword::Elif),
            token(Keyword::Fi),
            token(Keyword::Then),
        )))),
        swallow(peek(one_of("})"))),
        swallow(peek(token(ControlOp::DSemi))),
    ))(span)
}

fn pipeline(span: Span) -> ParseResult<Pipeline> {
    // Ensure there's not a misplaced token.
    let (span, _) = context("unexpected token", not(peek_sus_token))(span)?;

    // Parse a bang if there's one.
    let (span, _) = opt(|span| {
        let (span, trivia) = preceded(char('!'), trivia)(span)?;
        if trivia.is_empty() {
            span.extra
                .diag(ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace).range(&span));
        }

        Ok((span, ()))
    })(span)?;

    // Parse the commands.
    let (span, cmds) = separated_list1(
        context(
            "invalid pipeline operator!",
            delimited(
                // Make sure we match | and |& but not ||.
                not(token(ControlOp::OrIf)),
                alt((token(ControlOp::OrAnd), token(ControlOp::Or))),
                pair(trivia, linebreak),
            ),
        ),
        cmd,
    )(span)?;

    // Spacing and bye.
    let (span, _) = trivia(span)?;

    Ok((span, Pipeline::new(cmds)))
}

fn simple_cmd(span: Span) -> ParseResult<SimpleCmd> {
    fn let_seq(span: Span) -> ParseResult<CmdSuffixSgmt> {
        // Read the input math word.
        let (span, (mut start, seq_span)) =
            pair(position, terminated(recognize(word), trivia))(span)?;

        // Remove quotes.
        let (mut seq_span, quote) = opt(one_of("'\""))(seq_span)?;
        if let Some(quote) = quote {
            (seq_span, start) = position(seq_span)?;
            (_, seq_span) =
                all_consuming(terminated(take_till(|c| c == quote), char(quote)))(seq_span)?;
        }

        // Subparse the math expression.
        let seq_span = unsafe {
            Span::new_from_raw_offset(
                start.location_offset(),
                start.location_line(),
                &seq_span,
                ParseContext::new(),
            )
        };
        let (seq_span, seq) = all_consuming(arith_seq)(seq_span)?;
        span.extra.extend_diags(seq_span.extra);

        Ok((span, seq.into()))
    }

    // Parse the prefix and the command.
    let (mut span, (prefix, cmd)) = verify(
        pair(
            many0(alt((into(redir), into(assign)))),
            opt(delimited(
                // Ignore alias supression.
                opt(pair(
                    backslash,
                    peek(satisfy(|c| {
                        c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | ':')
                    })),
                )),
                ranged(word),
                trivia,
            )),
        ),
        |(prefix, cmd)| cmd.is_some() || !prefix.is_empty(),
    )(span)?;

    let mut suffix = vec![];
    if let Some((cmd, range)) = &cmd {
        // Check if the command looks like a comment.
        if let WordSgmt::Lit(lit) = &cmd.sgmts()[0] {
            if lit.starts_with("//") || {
                if let Some(WordSgmt::Glob(glob)) = &cmd.sgmts().get(1) {
                    lit == "/" && glob == "*"
                } else {
                    false
                }
            } {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                        .label("C-like comment", *range)
                        .help("In shell scripts, comments begin with #."),
                );
            }
        }

        if let Some(cmd) = cmd.as_lit() {
            // Not using the right keyword for an "else if" statement.
            if matches!(&*cmd.to_ascii_lowercase(), "elsif" | "elseif") {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::NotShellCode)
                        .range(*range)
                        .help("Use 'elif' to start another branch."),
                );
            }

            // When using "builtin", the first argument is the actual shell command.
            let cmd = if cmd == "builtin" {
                let builtin;
                (span, builtin) = opt(map(terminated(word, trivia), |word| {
                    String::from(word.as_lit().unwrap_or_default())
                }))(span)?;
                builtin.unwrap_or_default()
            } else {
                cmd.into()
            };

            // Get the suffix based on the command.
            (span, suffix) = match &*cmd {
                "declare" | "export" | "local" | "readonly" | "typeset" => {
                    many1(alt((
                        into(redir),
                        into(assign),
                        into(terminated(word, trivia)),
                    )))(span)?
                }
                "eval" => many1(alt((
                    into(redir),
                    into(terminated(word, trivia)),
                    cut(context(
                        "quote or escape your parens when using eval!",
                        preceded(char('('), fail),
                    )),
                )))(span)?,
                "let" => context(
                    "expected an expression for this let command!",
                    many1(alt((into(redir), let_seq, into(terminated(word, trivia))))),
                )(span)?,
                "time" => preceded(
                    // Read but ignore flag arguments.
                    many0(delimited(peek(char('-')), word, trivia)),
                    // Parse the input pipeline, if any.
                    map(opt(pipeline), |pipeline| {
                        pipeline.map_or(vec![], |pipeline| vinto![pipeline])
                    }),
                )(span)?,
                _ => many0(alt((into(redir), into(terminated(word, trivia)))))(span)?,
            }
        }
    }

    // TODO: readSource
    // TODO: syntaxCheckTrap

    // Drop the range.
    let cmd = cmd.map(|(cmd, _)| cmd);

    // Ignore flag arguments.
    suffix.retain(|sgmt| {
        if let CmdSuffixSgmt::Word(word) = sgmt {
            word.as_lit().map_or(true, |lit| !lit.starts_with('-'))
        } else {
            true
        }
    });

    Ok((span, SimpleCmd::new(cmd, prefix, suffix)))
}

pub(super) fn subshell(span: Span) -> ParseResult<Term> {
    delimited(
        // Opening parenthesis and whitespace:
        pair(char('('), multi_trivia),
        // The subshell's list:
        term,
        // Closing parenthesis and whitespace:
        tuple((
            multi_trivia,
            context("expected a closing )!", char(')')),
            trivia,
        )),
    )(span)
}

pub(super) fn term(span: Span) -> ParseResult<Term> {
    fn and_or(span: Span) -> ParseResult<Term> {
        // Left-fold the pipelines into a list.
        preceded(
            multi_trivia,
            map(
                pair(
                    map(pipeline, Term::Pipeline),
                    many0(separated_pair(
                        alt((token(ControlOp::AndIf), token(ControlOp::OrIf))),
                        pair(trivia, linebreak),
                        pipeline,
                    )),
                ),
                |(head, tail)| {
                    tail.into_iter().fold(head, |acc, (op, pipeline)| {
                        List::new(acc, pipeline, op).into()
                    })
                },
            ),
        )(span)
    }

    fn and_sep(span: Span) -> ParseResult<ControlOp> {
        let (span, (start, and)) = pair(position, token(ControlOp::And))(span)?;

        // Warn if the & seems to begin an HTML entity.
        let (mut span, entity_end) = opt(peek(preceded(
            pair(
                alt((
                    preceded(char('#'), digit1),
                    preceded(tag("#x"), alphanumeric1),
                    alpha1,
                )),
                char(';'),
            ),
            position,
        )))(span)?;
        if let Some(entity_end) = entity_end {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label("unquoted HTML entity", Range::new(&start, entity_end))
                    .help("Replace it with the corresponding character."),
            );
        } else {
            // Warn if the & might be ending a command by mistake (e.g. it is in a URL).
            let alpha_char;
            (span, alpha_char) =
                followed_by(satisfy(|c| c == '_' || c.is_ascii_alphabetic()))(span)?;
            if alpha_char {
                span.extra.diag(
                    ParseDiagnostic::builder(ParseDiagnosticKind::MissingSpace)
                        .label("this & terminates the command", Range::new(&start, &span))
                        .help("Quote or escape the & if you want a literal ampersand, or add a space after it."),
                );
            }
        }

        // Warn about redundant semicolons.
        let (span, semi_end) =
            preceded(trivia, opt(preceded(token(ControlOp::Semi), position)))(span)?;
        if let Some(semi_end) = semi_end {
            span.extra.diag(
                ParseDiagnostic::builder(ParseDiagnosticKind::SusToken)
                    .label(
                        "both & and ; terminate the command",
                        Range::new(start, semi_end),
                    )
                    .help("Just use one of them."),
            );
        }

        Ok((span, and))
    }

    fn sep(span: Span) -> ParseResult<ControlOp> {
        alt((
            terminated(
                alt((
                    // &, but not && or &>.
                    terminated(and_sep, not(one_of("&>"))),
                    // ;, but not clause operators.
                    terminated(token(ControlOp::Semi), not(one_of(";&"))),
                )),
                pair(trivia, linebreak),
            ),
            value(ControlOp::Newline, newline_list),
        ))(span)
    }

    let (span, term) = map(pair(and_or, many0(pair(sep, and_or))), |(head, tail)| {
        tail.into_iter()
            .fold(head, |acc, (op, and_or)| List::new(acc, and_or, op).into())
    })(span)?;

    // Read the optional terminator.
    let (span, _) = opt(sep)(span)?;

    Ok((span, term))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn test_brace_group() {
        // Space after the opening brace is required except if we have {(.
        assert_parse!(
            brace_group("{foo;}"),
            tests::pipeline("foo").into(),
            [((1, 2), ParseDiagnosticKind::MissingSpace)]
        );

        // TODO: Add a test for {()}
        // The space before the } can be omitted if there's a semicolon.
        assert_parse!(brace_group("{ foo;}"), tests::pipeline("foo").into());

        // Empty code blocks aren't allowed.
        assert_parse!(brace_group("{ }") => Err(
            (1, 3),
            Notes: [((1, 3), "unexpected token")],
            Diags: [((1, 1), (1, 4), ParseDiagnosticKind::SusToken)]
        ));

        // The closing curly needs a semicolon or newline before it.
        assert_parse!(brace_group("{ foo}") => Err(
            (1, 7),
            Notes: [((1, 7), "missing the closing brace")],
            Diags: [((1, 6), (1, 7), ParseDiagnosticKind::SusToken)]
        ));
    }

    #[test]
    fn test_case_clause() {
        // The opening parenthesis is optional.
        assert_parse!(
            case_clause("foo) bar;;"),
            CaseClause::new(
                vec![tests::word("foo")],
                Some(tests::pipeline("bar").into()),
                Some(ClauseSep::Break),
            )
        );

        // There can be space between the patterns.
        assert_parse!(
            case_clause("(  foo  |\tbar) baz;;&"),
            CaseClause::new(
                vec![tests::word("foo"), tests::word("bar")],
                Some(tests::pipeline("baz").into()),
                Some(ClauseSep::Continue),
            )
        );

        // The only invalid keyword is 'esac'.
        assert_parse!(
            case_clause("if | do ) foo ;;"),
            CaseClause::new(
                vec![tests::word("if"), tests::word("do")],
                Some(tests::pipeline("foo").into()),
                Some(ClauseSep::Break),
            )
        );
        assert_parse!(
            case_clause("( esac ) foo ;&"),
            CaseClause::new(
                vec![tests::word("esac")],
                Some(tests::pipeline("foo").into()),
                Some(ClauseSep::Fallthrough),
            ),
            [((1, 3), (1, 7), ParseDiagnosticKind::SusToken)]
        );

        // The last clause can omit the separator.
        assert_parse!(
            case_clause("foo) bar\nesac") => "esac",
            CaseClause::new(
                vec![tests::word("foo")],
                Some(tests::pipeline("bar").into()),
                None,
            )
        );

        // Missing the separator between clauses.
        assert_parse!(case_clause("foo) bar\nbaz) qux") => Err(
            (2, 4),
            Notes: [((2, 4), "did you forget the separator")]
        ));

        // Missing the closing parenthesis.
        assert_parse!(case_clause("foo | bar") => Err(
            (1, 10),
            Notes: [((1, 10), "expected a closing ')'")]
        ));
    }

    #[test]
    fn test_case_cmd() {
        // There can be horizontal/vertical whitespace before the 'in'.
        assert_parse!(
            case_cmd("case foo in bar) baz;; esac"),
            CaseCmd::new(
                tests::word("foo"),
                vec![CaseClause::new(
                    vec![tests::word("bar")],
                    Some(tests::pipeline("baz").into()),
                    Some(ClauseSep::Break),
                )],
            )
        );
        assert_parse!(
            case_cmd("case foo in\n\tbar) baz;;\nesac"),
            CaseCmd::new(
                tests::word("foo"),
                vec![CaseClause::new(
                    vec![tests::word("bar")],
                    Some(tests::pipeline("baz").into()),
                    Some(ClauseSep::Break),
                )],
            )
        );

        // There can be no clauses.
        assert_parse!(
            case_cmd("case foo in\nesac"),
            CaseCmd::new(tests::word("foo"), vec![])
        );

        // Missing keywords.
        assert_parse!(case_cmd("case foo\nbar) baz;;\nesac") => Err(
            (2, 1),
            Notes: [((2, 1), "expected 'in'")]
        ));
        assert_parse!(case_cmd("case foo in\nbar) baz;;") => Err(
            (2, 11),
            Notes: [((2, 11), "expected a closing 'esac'")]
        ));
    }

    #[test]
    fn test_compound_command() {
        // Command with bad redirection.
        assert_parse!(
            compound_cmd("{ foo; }>bar baz") => "baz",
            CompoundCmd::new(
                Construct::BraceGroup(tests::pipeline("foo").into()),
                vec![Redir::new(None, RedirOp::Great, tests::word("bar"))],
            ),
            [((1, 14), (1, 17), ParseDiagnosticKind::SusToken)]
        );
        // ...but don't trigger it for keywords.
        assert_parse!(
            compound_cmd("{ foo; }>bar do") => "do",
            CompoundCmd::new(
                Construct::BraceGroup(tests::pipeline("foo").into()),
                vec![Redir::new(None, RedirOp::Great, tests::word("bar"))],
            )
        );

        // TODO: Test for ambiguous ((..)).
    }

    #[test]
    fn test_cond() {
        // Operators can be escaped or quoted.
        assert_parse!(
            cond("[ foo \\< bar ]"),
            Cond::new(
                true,
                Some(BinExpr::new(tests::word("foo"), tests::word("bar"), BinOp::Lt).into()),
            )
        );
        assert_parse!(
            cond("[ foo '<' bar ]"),
            Cond::new(
                true,
                Some(BinExpr::new(tests::word("foo"), tests::word("bar"), BinOp::Lt).into()),
            )
        );

        // Same with parentheses.
        assert_parse!(
            cond("[ '(' foo \\) ]"),
            Cond::new(
                true,
                Some(CondExpr::Group(Box::new(tests::word("foo").into())))
            )
        );
        // ...In fact, you have to do it inside single brackets.
        assert_parse!(
            cond("[ '(' foo ) ]"),
            Cond::new(
                true,
                Some(CondExpr::Group(Box::new(tests::word("foo").into())))
            ),
            [((1, 11), (1, 12), ParseDiagnosticKind::MissingEscape)]
        );
        // ...But in double brackets is wrong to do it.
        assert_parse!(
            cond("[[ '(' foo ) ]]"),
            Cond::new(
                false,
                Some(CondExpr::Group(Box::new(tests::word("foo").into())))
            ),
            [((1, 4), (1, 7), ParseDiagnosticKind::BadEscape)]
        );

        // New lines have to be escaped inside single brackets.
        assert_parse!(
            cond("[\n]"),
            Cond::new(true, None),
            [((1, 2), (2, 1), ParseDiagnosticKind::MissingEscape)]
        );

        // Empty conditions are legal.
        assert_parse!(cond("[[ ]]"), Cond::new(false, None));

        // Bracket mismatch.
        assert_parse!(
            cond("[ ]]"),
            Cond::new(true, None),
            [((1, 3), (1, 5), ParseDiagnosticKind::SusToken)]
        );

        // if [ grep foo file ] pitfall.
        assert_parse!(cond("[ grep foo file ]") => Err(
            (1, 8),
            Notes: [((1, 8), "expected the test to end")],
            Diags: [((1, 3), (1, 7), ParseDiagnosticKind::NotShellCode)]
        ));

        // Incorrect opening bracket.
        assert_parse!(cond("]") => Err(1, 1));

        // Missing the closing bracket.
        assert_parse!(cond("[ ") => Err(
            (1, 3),
            Notes: [((1, 3), "invalid or missing word"), ((1, 3), "invalid condition argument")]
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
            Diags: [((1, 3), (1, 4), ParseDiagnosticKind::SusToken)]
        ));

        // Shouldn't use math operators inside test conditions.
        assert_parse!(cond("[ x + 1 ]") => Err(
            (1, 5),
            Notes: [((1, 5), "expected the test to end")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::SusToken)]
        ));
        assert_parse!(cond("[ x - 1 ]") => Err(
            (1, 6),
            Notes: [((1, 6), "expected a test operator")],
            Diags: [((1, 5), (1, 6), ParseDiagnosticKind::SusToken)]
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

    #[test]
    fn test_cond_cmd() {
        // Valid condition commands.
        assert_parse!(
            cond_cmd("[[ foo ]]"),
            CompoundCmd::new(Cond::new(false, Some(tests::word("foo").into())), vec![])
        );
        assert_parse!(
            cond_cmd("[ foo ] > bar"),
            CompoundCmd::new(
                Cond::new(true, Some(tests::word("foo").into())),
                vec![Redir::new(None, RedirOp::Great, tests::word("bar"))]
            )
        );

        // Wrong operator between conditions.
        assert_parse!(
            cond_cmd("[ foo ] -a [ bar ]") => "-a [ bar ]",
            CompoundCmd::new(Cond::new(true, Some(tests::word("foo").into())), vec![]),
            [((1, 9), (1, 11), ParseDiagnosticKind::BadOperator)]
        );

        // Missing the operator between conditions.
        assert_parse!(
            cond_cmd("[ foo ] [ bar ]") => "[ bar ]",
            CompoundCmd::new(
                Cond::new(true, Some(tests::word("foo").into())),
                vec![],
            ),
            [((1, 9), (1, 10), ParseDiagnosticKind::SusToken)]
        );
    }

    #[test]
    fn test_coproc() {
        // The name is optional.
        assert_parse!(
            coproc("coproc foo { bar; }"),
            Coproc::new(
                Some("foo".into()),
                CompoundCmd::new(Construct::BraceGroup(tests::pipeline("bar").into()), vec![])
            )
        );
        assert_parse!(
            coproc("coproc { foo; }"),
            Coproc::new(
                None,
                CompoundCmd::new(Construct::BraceGroup(tests::pipeline("foo").into()), vec![])
            )
        );

        // Using a simple command.
        assert_parse!(
            coproc("coproc foo bar"),
            Coproc::new(
                None,
                SimpleCmd::new(Some(tests::word("foo")), vec![], vinto![tests::word("bar")])
            )
        );
    }

    #[test]
    fn test_do_group() {
        // Warn about 'do;s'.
        assert_parse!(
            do_group("do; { foo; } done"),
            Pipeline::new(vinto![CompoundCmd::new(
                Construct::BraceGroup(tests::pipeline("foo").into()),
                vec![],
            )])
            .into(),
            [((1, 3), (1, 4), ParseDiagnosticKind::BadOperator)]
        );

        // Looks like a procsub follows.
        assert_parse!(
            do_group("do { foo; } done <(bar)") => "<(bar)",
            Pipeline::new(vinto![
                CompoundCmd::new(
                    Construct::BraceGroup(tests::pipeline("foo").into()),
                    vec![],
                )
            ]).into(),
            [((1, 18), (1, 20), ParseDiagnosticKind::SusToken)]
        );

        // Empty groups aren't valid.
        assert_parse!(do_group("do done") => Err(
            (1, 4),
            Notes: [((1, 4), "empty 'do'")]
        ));
    }

    #[test]
    fn test_for_loop() {
        // The body can be a brace group.
        assert_parse!(
            for_loop("for foo in *; { bar; }"),
            InListed::new(
                "foo",
                vec![Word::new(vec![WordSgmt::Glob("*".into())])],
                Pipeline::new(vinto![tests::cmd("bar")]),
            )
            .into()
        );

        // The in-list can be empty.
        assert_parse!(
            for_loop("for foo; do bar; done"),
            InListed::new("foo", vec![], tests::pipeline("bar")).into()
        );

        // Handle spacing with arithmetic for-loops, warning if there's
        // whitespace between the header parens.
        assert_parse!(
            for_loop("for (( i=0;\ti<5; i++ ) ); do foo; done"),
            ArithForLoop::new(
                (
                    vinto![BinExpr::new(
                        tests::var("i"),
                        tests::arith_number("0"),
                        BinOp::Eq,
                    )],
                    vinto![BinExpr::new(
                        tests::var("i"),
                        tests::arith_number("5"),
                        BinOp::Lt
                    )],
                    vinto![UnExpr::new(tests::var("i"), UnOp::Inc)]
                ),
                tests::pipeline("foo"),
            )
            .into(),
            [((1, 23), (1, 24), ParseDiagnosticKind::BadSpace)]
        );

        // The header sections can be empty.
        assert_parse!(
            for_loop("for ((;;)); do foo; done"),
            ArithForLoop::new((vec![], vec![], vec![]), tests::pipeline("foo")).into()
        );
    }

    #[test]
    fn test_if_cmd() {
        fn if_block(cond: &str, branch: &str) -> CondBlock {
            CondBlock::new(tests::pipeline(cond), tests::pipeline(branch))
        }

        // Vanilla if-else.
        assert_parse!(
            if_cmd("if true; then\nfoo\nelse bar\nfi"),
            IfCmd::new(
                if_block("true", "foo"),
                vec![],
                Some(tests::pipeline("bar").into()),
            )
        );

        // Handle multiple 'elif' branches and a missing 'else' branch.
        assert_parse!(
            if_cmd("if 0; then\nfoo\nelif 1; then\nbar\n elif 2\nthen baz\nfi"),
            IfCmd::new(
                if_block("0", "foo"),
                vec![if_block("1", "bar"), if_block("2", "baz")],
                None,
            )
        );

        // Warn about 'then;'s.
        assert_parse!(
            if_cmd("if true; then; foo; fi"),
            IfCmd::new(if_block("true", "foo"), vec![], None),
            [((1, 14), (1, 15), ParseDiagnosticKind::BadOperator)]
        );

        // Warn when using 'else if' in the same line.
        assert_parse!(
            if_cmd("if 1; then foo; else if 2; then bar; fi fi"),
            IfCmd::new(
                if_block("1", "foo"),
                vec![],
                Some(
                    Pipeline::new(vinto![CompoundCmd::new(
                        IfCmd::new(if_block("2", "bar"), vec![], None),
                        vec![]
                    )])
                    .into()
                )
            ),
            [((1, 17), (1, 24), ParseDiagnosticKind::NotShellCode)]
        );
        // ...but don't if it's on a separate line.
        assert_parse!(
            if_cmd("if 1; then foo; else\nif 2; then bar; fi fi"),
            IfCmd::new(
                if_block("1", "foo"),
                vec![],
                Some(
                    Pipeline::new(vinto![CompoundCmd::new(
                        IfCmd::new(if_block("2", "bar"), vec![], None),
                        vec![]
                    )])
                    .into()
                )
            )
        );

        // Missing keywords.
        assert_parse!(if_cmd("if true; foo; fi") => Err(
            (1, 15),
            Notes: [((1, 15), "expected 'then'")]
        ));
        assert_parse!(if_cmd("if true; then foo") => Err(
            (1, 18),
            Notes: [((1, 18), "expected 'fi'")]
        ));

        // Empty clauses.
        assert_parse!(if_cmd("if true; then\nfi") => Err(
            (2, 1),
            Notes: [((2, 1), "empty 'then'")]
        ));
        assert_parse!(if_cmd("if true; then foo; else\nfi") => Err(
            (2, 1),
            Notes: [((2, 1), "empty 'else'")]
        ));
    }

    #[test]
    fn test_in_list() {
        // Lists with the right terminator:
        assert_parse!(
            in_list("in foo bar;"),
            vec![tests::word("foo"), tests::word("bar")]
        );
        assert_parse!(
            in_list("in foo bar  \n  "),
            vec![tests::word("foo"), tests::word("bar")]
        );

        // Inlined 'do's should emit a lint.
        assert_parse!(
            in_list("in foo bar do baz") => "do baz",
            vec![tests::word("foo"), tests::word("bar")],
            [((1, 12), ParseDiagnosticKind::MissingSpace)]
        );
    }

    #[test]
    fn test_in_listed() {
        // Dollar in iterator variable.
        assert_parse!(
            in_listed("$foo in bar; { baz; }"),
            InListed::new("foo", vec![tests::word("bar")], tests::pipeline("baz"),),
            [((1, 1), (1, 5), ParseDiagnosticKind::NotShellCode)]
        );

        // The in-list can be empty.
        assert_parse!(
            in_listed("foo in; { bar; }"),
            InListed::new("foo", vec![], tests::pipeline("bar"))
        );
    }

    #[test]
    fn test_pipeline() {
        // A single command is a valid pipeline.
        assert_parse!(pipeline("! ls"), Pipeline::new(vinto![tests::cmd("ls")]));

        // Make sure we can handle new lines between commands.
        assert_parse!(
            pipeline("ls |\n\ngrep .txt"),
            Pipeline::new(vinto![
                tests::cmd("ls"),
                SimpleCmd::new(
                    Some(tests::word("grep")),
                    vec![],
                    vinto![tests::word(".txt")],
                )
            ])
        );

        // Pipelines aren't separated with ||.
        assert_parse!(pipeline("ls || grep .txt") => "|| grep .txt", tests::pipeline("ls"));

        // Error when the pipeline begins with a sus keyword.
        assert_parse!(pipeline("then") => Err((1, 1), Notes: [((1, 1), "unexpected")]));
    }

    #[test]
    fn test_simple_cmd() {
        // Ignore backslashes for alias supressions.
        assert_parse!(simple_cmd("\\ls"), tests::cmd("ls"));

        // Can handle just a prefix.
        assert_parse!(
            simple_cmd("foo=bar"),
            SimpleCmd::new(
                None,
                vinto![Assign::new(
                    tests::var("foo"),
                    tests::word("bar"),
                    BinOp::Eq
                )],
                vec![],
            )
        );

        // Prefixes can contain assignments or redirections.
        assert_parse!(
            simple_cmd("foo=bar >file ls"),
            SimpleCmd::new(
                Some(tests::word("ls")),
                vinto![
                    Assign::new(tests::var("foo"), tests::word("bar"), BinOp::Eq),
                    Redir::new(None, RedirOp::Great, tests::word("file"))
                ],
                vec![],
            )
        );

        // Warn when the command looks like a C-like comment.
        assert_parse!(
            simple_cmd("// echo"),
            SimpleCmd::new(Some(tests::word("//")), vec![], vinto![tests::word("echo")]),
            [((1, 1), (1, 3), ParseDiagnosticKind::NotShellCode)]
        );
        assert_parse!(
            simple_cmd("/* echo */") => "echo */",
            SimpleCmd::new(
                Some(Word::new(vec![WordSgmt::Lit("/".into()), WordSgmt::Glob("*".into())])),
                vec![],
                vec![]
            ),
            [((1, 1), (1, 3), ParseDiagnosticKind::NotShellCode)]
        );

        // Not using the right "else if" keyword.
        assert_parse!(
            simple_cmd("elseif"),
            tests::cmd("elseif"),
            [((1, 1), (1, 7), ParseDiagnosticKind::NotShellCode)]
        );

        // let expressions (which can be quoted or not).
        assert_parse!(
            simple_cmd("let x=0"),
            SimpleCmd::new(
                Some(tests::word("let")),
                vec![],
                vinto![vinto![BinExpr::new(
                    tests::var("x"),
                    tests::arith_number("0"),
                    BinOp::Eq
                )]]
            )
        );
        assert_parse!(
            simple_cmd("let \"x = 0\""),
            SimpleCmd::new(
                Some(tests::word("let")),
                vec![],
                vinto![vinto![BinExpr::new(
                    tests::var("x"),
                    tests::arith_number("0"),
                    BinOp::Eq
                )]]
            )
        );

        // let expressions need arguments.
        assert_parse!(simple_cmd("let") => Err(
            (1, 4),
            Notes: [((1, 4), "invalid or missing word"), ((1, 4), "expected an expression")]
        ));
    }

    #[test]
    fn test_subshell() {
        // Since parentheses are operators, they don't need to be separated
        // from the list by whitespace.
        assert_parse!(subshell("(foo)"), tests::pipeline("foo").into());
    }

    #[test]
    fn test_term() {
        // Handle line continuations and lists of lists.
        assert_parse!(
            term("ls;\\\necho 'foo' && echo 'bar'"),
            List::new(
                tests::pipeline("ls"),
                List::new(
                    Pipeline::new(vinto![SimpleCmd::new(
                        Some(tests::word("echo")),
                        vec![],
                        vinto![Word::new(vec![WordSgmt::SingleQuoted("foo".into())])]
                    )]),
                    Pipeline::new(vinto![SimpleCmd::new(
                        Some(tests::word("echo")),
                        vec![],
                        vinto![Word::new(vec![WordSgmt::SingleQuoted("bar".into())])]
                    )]),
                    ControlOp::AndIf,
                ),
                ControlOp::Semi,
            )
            .into()
        );

        // Warn when there's an HTML entity.
        assert_parse!(
            term("foo &amp;&amp; bar") => ";&amp; bar",
            List::new(
                tests::pipeline("foo"),
                tests::pipeline("amp"),
                ControlOp::And
            ).into(),
            [((1, 5), (1, 10), ParseDiagnosticKind::SusToken)]
        );

        // Check if something looks like URL query parameters.
        assert_parse!(
            term("com&q=foo"),
            List::new(
                tests::pipeline("com"),
                Pipeline::new(vinto![SimpleCmd::new(
                    None,
                    vinto![Assign::new(tests::var("q"), tests::word("foo"), BinOp::Eq)],
                    vec![],
                )]),
                ControlOp::And,
            )
            .into(),
            [((1, 4), (1, 5), ParseDiagnosticKind::MissingSpace)]
        );
    }
}
