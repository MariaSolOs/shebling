use super::*;

fn bracketed_glob(span: ParseSpan) -> ParseResult<String> {
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
                        recognize_string(one_of(&*format!("[{}", expansion::EXTGLOB_PREFIX))),
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

            format!("[{}]", sgmts.concat())
        },
    )(span)
}

fn lit_string(end_pattern: &'static str) -> impl Fn(ParseSpan) -> ParseResult<String> {
    move |span| {
        map(
            many1(alt((
                // Escaped characters.
                preceded(
                    backslash,
                    into(alt((
                        line_space,
                        one_of(&*format!(
                            "|&;<>()'\n\r[]{{}}.,~#{}{}",
                            expansion::EXTGLOB_PREFIX,
                            quoted::DOUBLE_ESCAPABLE
                        )),
                    ))),
                ),
                // Some other sequence of non-special, unescaped characters.
                recognize_string(is_not(&*format!(
                    "[{{}}|&;<>() '\t\n\r\u{A0}{}{}{}",
                    expansion::EXTGLOB_PREFIX,
                    quoted::DOUBLE_ESCAPABLE,
                    end_pattern
                ))),
            ))),
            |lits| lits.concat(),
        )(span)
    }
}

pub(super) fn lit_word_sgmt<'a>(
    end_pattern: &'static str,
) -> impl FnMut(ParseSpan<'a>) -> ParseResult<WordSgmt> {
    map(spanned(lit_string(end_pattern)), WordSgmt::Lit)
}

pub(super) fn identifier(span: ParseSpan) -> ParseResult<String> {
    recognize_string(pair(
        // Make sure the first character is not a number.
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(span)
}

pub(super) fn word_sgmt(span: ParseSpan) -> ParseResult<WordSgmt> {
    word_sgmt_before_pattern("")(span)
}

fn word_sgmt_before_pattern<'a>(
    pattern: &'static str,
) -> impl FnMut(ParseSpan<'a>) -> ParseResult<WordSgmt> {
    alt((
        map(spanned(single_quoted), WordSgmt::SingleQuoted),
        into(double_quoted),
        map(
            spanned(alt((
                extglob,
                recognize_string(one_of("*?")),
                bracketed_glob,
            ))),
            WordSgmt::Glob,
        ),
        // Fallback for other glob prefix characters:
        map(spanned(recognize_string(one_of("@!+["))), WordSgmt::Lit),
        dollar_sgmt,
        map(brace_expansion, WordSgmt::BraceExpansion),
        // TODO: map(backquoted, WordSgmt::BackQuoted),
        // TODO: map(proc_sub, WordSgmt::ProcSub),
        lit_word_sgmt(pattern),
        // Literal curly braces:
        map(
            spanned(recognize_string(alt((tag("{}"), tag("{"), tag("}"))))),
            WordSgmt::Lit,
        ),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests;

    use insta::assert_debug_snapshot;

    #[test]
    fn test_identifier() {
        // Valid identifiers.
        assert_debug_snapshot!(tests::parse_ok(identifier, "foo", ""), @r###"
        (
            [],
            "foo",
        )
        "###);
        assert_debug_snapshot!(tests::parse_ok(identifier, "_foo0", ""), @r###"
        (
            [],
            "_foo0",
        )
        "###);

        // Identifiers cannot start with a number.
        assert_debug_snapshot!(tests::parse_fail(identifier, "0foo"), @r###"
        (
            ParseError {
                location: 0,
                notes: [],
            },
            [],
        )
        "###);
    }
}
