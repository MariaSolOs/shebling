use crate::ast::Range;
use paste::paste;

// TODO: Create linting severities.

#[derive(Clone, Debug)]
pub(crate) struct LintMeta {
    code: &'static str,
    message_format: &'static str,
}

#[derive(Clone, Debug)]
pub(crate) struct Lint {
    range: Range,
    meta: LintMeta,
    message: String,
}

impl Lint {
    pub(super) fn new(range: Range, meta: LintMeta) -> Self {
        let message = String::from(meta.message_format);
        Self::with_message(range, meta, message)
    }

    pub(super) fn with_message(range: Range, meta: LintMeta, message: impl Into<String>) -> Self {
        Self {
            range,
            meta,
            message: message.into(),
        }
    }

    pub(crate) fn range(&self) -> &Range {
        &self.range
    }

    pub(crate) fn code(&self) -> &'static str {
        self.meta.code
    }
}

impl PartialEq for Lint {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
            && self.meta.code == other.meta.code
            && self.message == other.message
    }
}

macro_rules! define_meta {
    ($($name:ident $message_format:literal $([$($arg:ident: $ty:ty),+])?)+) => {
        $(
            pub(crate) const $name: self::LintMeta = self::LintMeta {
                code: stringify!($name),
                message_format: $message_format,
            };

            $(paste! {
                pub(crate) fn [<format_ $name:lower>]($($arg: $ty),+) -> String {
                    format!($message_format, $($arg),+)
                }
            })?
        )+
    };
}

// Ordered alphabetically by code.
// TODO: Make this specific to each parser when creating the new parser crate.
// TODO: Create a subdiagnostic logic. Some of these could be merged into a single lint.
define_meta! {
    AMP_SEMI "Both `&` and `;` terminate commands. You should only use one of them."
    APOSTROPHE "This apostrophe terminated the single quoted string!"
    BRACKETED_IF "`[` is a command, not a syntax marker for the if statement!"
    BS_TRAILING_SPACE "Delete trailing space after `\\` for a line break (or add quotes for a literal space)."
    COMMENTED_BS_LF "Not a line continuation, this backslash is part of a comment!"
    COMPARATOR_IN_MATH "For math stuff, use `{}` instead of `{}`." [math_op: &str, test_op: &str]
    COND_BRACKET_MISMATCH "This closing bracket doesn't match the opening one."
    C_LIKE_COMMENT "This looks like a `C` comment. In shell scripts, comments begin with `#`."
    DISALLOWED_SEMI "You shouldn't have a semicolon after `{}`." [keyword: &str]
    DOLLAR_IN_LIST_NAME "Don't prefix iterator variables with `$`!"
    DONE_PROC_SUB "Use `done < <(cmd)` to redirect from process substitution (you're missing one `<`)."
    ELIF_LIKE "Use `elif` to start another branch."
    EMPTY_BLOCK "Empty code block! If intended, use a no-op."
    FORWARD_TICKED "For command expansion, use backticks (``)."
    HTML_ENTITY "Unquoted HTML entity! Replace it with the corresponding character."
    IGNORING_BS "This `\\{0}` will be a vanilla `{0}` here (the backslash will be ignored)." [c: char]
    INLINED_DO "You need a new line or a semicolon before `do`."
    LITERAL_CR "Oh no, literal `\\r`. Try running the script through `tr -d '\\r'`."
    LITERAL_KEYWORD "Literal `{}`! If intended, quote it. Else add a `;` or a `\\n` before it." [keyword: &str]
    MATH_IN_TEST "Inside test conditions, do math stuff inside `$((..))`."
    MISSING_SPACE "You need a space here!"
    MISUSED_EQEQ "Unexpected `==`! Use a single `=` for assignments, or `[..]`/`[[..]]` for comparisons. If intended, use quotes."
    NESTED_ARR "Doing math? Then you're missing the dollar in `$((..))`. Else use `( (` for nested arrays."
    OUTER_FLAG_OP "Use `&&` or `||` to join test commands."
    PARAM_LIST "No need to have a parameter list! Just use `()` and refer to them as `$1`, `$2`..."
    SPACED_FOR_PAREN "Remove spacing between `{0}{0}` in this arithmetic for-loop." [paren: char]
    SPACE_AFTER_EQ "If you want to assign a value, remove the space after `=` (or use `''` for an empty string)."
    STARTING_CONTROL "Unexpected operator! `{}` should be at the end of the previous line." [op: &str]
    SUS_CHAR_AFTER_QUOTE "This is actually an end quote, but due to the next char it looks kinda sus."
    SUS_POST_COND "Unexpected parameters after condition. Missing `&&`/`||`, or did you f*ck up?"
    TEST_GROUP "If grouping expressions inside `{}`, use `{}`." [test: &str, group: &str]
    TRAILING_POST_CMD "Weird stuff after compound command! Bad redirections or missing an operator?"
    UNBRACED_INDEX "Use braces when expanding arrays."
    UNBRACED_POSITIONAL "Braces are required for positionals over 9."
    UNCLOSED_STRING "Did you forget to close this string?"
    UNESCAPED_DOLLAR "Prefer escaping over ending quotes to make `$` literal."
    UNESCAPED_SINGLE_QUOTE "Wanna escape a single quote? 'Let'\\''s do it correctly.'"
    UNESCAPED_TEST_LF "You need to escape line breaks inside `[..]`."
    UNESCAPED_WHITESPACE "`\\{0}` is literally a `{0}` here. For a {1}, use {2} instead." [c: char, name: &str, alt: &str]
    UNESCAPED_COND_GROUP "Inside `[..]`, use `\\( .. \\)` (escape your parens!) or combine `[..]` expressions (even better)."
    UNNECESSARY_COND_GROUP_ESCAPE "Inside `[[..]]`, you shouldn't escape `(` or `)`."
    UNSPACED_AMP "This `&` terminates the command! Escape it or add a space after it."
    UNICHAR "This is a unicode {}! You may want to delete it and retype it." [name: &str]
    UNSPECIAL_ESCAPE "This character has no special behavior when escaped inside double quotes."
}
