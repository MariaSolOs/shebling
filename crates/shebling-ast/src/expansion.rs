use derive_more::From;

use super::*;

/// Shell expansions.
#[derive(Debug, From)]
pub enum DollarExp {
    /// Arithmetic expansion. Can happen inside `$(())` or `$[]`.
    Arith(ArithSeq),

    /// `ksh`-style command expansion (e.g. `${ foo; }`).
    // TODO: CmdExpansion(Term),

    /// Dollar-prefixed command substitution (e.g. `$(foo)`).
    ///
    /// The contained [Term] will be [None] iff the command substitution is `$()`.
    // TODO: CmdSub(Option<Term>),

    /// `$""` expression, used for locale-specific translation.
    DoubleQuoting(DoubleQuoted),

    /// Shell parameter expansion (e.g. `${!foo[@]}`).
    ParamExpansion(Vec<WordSgmt>),

    /// `$''` expression, used for ANSI-C quoting.
    #[from(ignore)]
    SingleQuoting(Spanned<String>),

    /// A variable.
    #[from(ignore)]
    Var(Spanned<String>),
}
