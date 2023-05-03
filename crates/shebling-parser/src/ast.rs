use shebling_codegen::{from_structs, New};

// region: Tokens.
/// Trait for types that represent single tokens, such as keywords
/// and operators.
pub(crate) trait Token
where
    Self: Copy + Sized,
{
    fn token(&self) -> &'static str;
}

/// Utility macro for creating [Token] enums.
macro_rules! tokenizable {
    (
         $(#[doc = $enum_doc:expr])*
         enum $name:ident {
             $($(#[doc = $arm_doc:expr])* $arm:ident($token:literal),)+
         }
     ) => {
         $(#[doc = $enum_doc])*
         #[derive(Clone, Copy, Debug, PartialEq)]
         pub(crate) enum $name {
             $(
                 /// ```sh
                 #[doc = $token]
                 /// ```
                 ///
                 $(#[doc = $arm_doc])*
                 $arm
             ),+
         }

         impl self::Token for $name {
             fn token(&self) -> &'static str {
                 match self {
                     $(Self::$arm => $token),+
                 }
             }
         }
     };
 }
// endregion

// region: Binary and unary expressions.
tokenizable! {
    enum BinOp {
        /// Addition operator.
        Add("+"),
        /// Addition assignment operator.
        AddEq("+="),
        /// Logical-and operator.
        And("&&"),
        /// Bitwise-and operator.
        BitAnd("&"),
        /// Bitwise-and assignment operator.
        BitAndEq("&="),
        /// Bitwise-or operator.
        BitOr("|"),
        /// Bitwise-or assignment operator.
        BitOrEq("|="),
        /// Bitwise-xor operator.
        BitXor("^"),
        /// Bitwise-xor assignment operator.
        BitXorEq("^="),
        /// Division operator.
        Div("/"),
        /// Division assignment operator.
        DivEq("/="),
        /// Assignment operator. In conditional expressions, it is also
        /// used for string equality.
        Eq("="),
        /// Equality operator. In conditional expressions, it is also
        /// used for string equality and pattern matching.
        EqEq("=="),
        /// Greater-than-or-equal-to operator.
        Ge(">="),
        /// Greater-than operator.
        Gt(">"),
        /// Less-than-or-equal-to operator.
        Le("<="),
        /// Less-than operator.
        Lt("<"),
        /// Regex match operator.
        Match("=~"),
        /// Modulus operator.
        Mod("%"),
        /// Modulus assignment operator.
        ModEq("%="),
        /// Multiplication operator.
        Mult("*"),
        /// Multiplication assignment operator.
        MultEq("*="),
        /// Not-equal-to operator.
        Ne("!="),
        /// Logical-or operator.
        Or("||"),
        /// Exponentiation operator.
        Pow("**"),
        /// Left-shift operator.
        Shl("<<"),
        /// Left-shift assignment operator.
        ShlEq("<<="),
        /// Right-shift operator.
        Shr(">>"),
        /// Right-shift assignment operator.
        ShrEq(">>="),
        /// Subtraction operator.
        Sub("-"),
        /// Subtraction assignment operator.
        SubEq("-="),
    }
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct BinExpr<O, L, R = L> {
    #[new(into)]
    left: Box<L>,
    #[new(into)]
    right: Box<R>,
    #[new(into)]
    op: O,
}

tokenizable! {
    enum UnOp {
        /// Bitwise negation operator.
        BitNeg("~"),
        /// Pre/post decrement operator.
        Dec("--"),
        /// Pre/post increment operator.
        Inc("++"),
        /// Unary minus operator.
        Neg("-"),
        /// Logical negation operator.
        Not("!"),
        /// Unary plus operator.
        Pos("+"),
    }
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct UnExpr<O, E> {
    #[new(into)]
    expr: Box<E>,
    #[new(into)]
    op: O,
}
// endregion

// region: Arithmetic expressions.
#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum ArithTerm {
    BinExpr(ArithBinExpr),
    Expansion(ArithExpansion),
    Group(ArithGroup),
    TriExpr(ArithTriExpr),
    UnExpr(ArithUnExpr),
    Variable(Variable),
}

pub(crate) type ArithBinExpr = BinExpr<BinOp, ArithTerm>;

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithExpansion {
    sgmts: Vec<WordSgmt>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithGroup {
    seq: ArithSeq,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithSeq {
    terms: Vec<ArithTerm>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct ArithTriExpr {
    #[new(into)]
    cond: Box<ArithTerm>,
    #[new(into)]
    then_branch: Box<ArithTerm>,
    #[new(into)]
    else_branch: Box<ArithTerm>,
}

pub(crate) type ArithUnExpr = UnExpr<UnOp, ArithTerm>;
// endregion

// region: Shell expansions.
#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum DollarExp {
    Arith(ArithSeq),
    // TODO CmdExpansion(DollarCmdExpansion),
    // TODO CmdSub(DollarCmdSub),
    // TODO DoubleQuoting(DoubleQuoted),
    ParamExpansion(ParamExpansion),
    // TODO SingleQuoting(SingleQuoted),
    Variable(Variable),
}

#[derive(Debug, New, PartialEq)]
pub struct ParamExpansion {
    sgmts: Vec<WordSgmt>,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Glob {
    #[new(into)]
    pattern: String,
}
// endregion

// region: Quoted strings.
#[derive(Debug, New, PartialEq)]
pub(crate) struct SingleQuoted {
    #[new(into)]
    string: String,
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct DoubleQuoted {
    sgmts: Vec<DoubleQuotedSgmt>,
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum DoubleQuotedSgmt {
    // TODO BackQuoted(BackQuoted),
    Lit(Lit),
    DollarExp(DollarExp),
}
// endregion

// region: Words.
#[derive(Debug, New, PartialEq)]
pub(crate) struct Word {
    sgmts: Vec<WordSgmt>,
}

impl Word {
    pub(crate) fn sgmts(&self) -> &[WordSgmt] {
        &self.sgmts
    }

    /// If this [Word] represents a literal string, returns the [Lit].
    ///
    /// A word is considered a literal if:
    /// - The word has a single segment.
    /// - Such segment is a [WordSgmt::Lit].
    pub(crate) fn as_lit(&self) -> Option<&Lit> {
        match &self.sgmts.first() {
            Some(WordSgmt::Lit(lit)) if self.sgmts.len() == 1 => Some(lit),
            _ => None,
        }
    }
}

#[from_structs]
#[derive(Debug, PartialEq)]
pub(crate) enum WordSgmt {
    // TODO BackQuoted(BackQuoted),
    // TODO BraceExpansion(BraceExpansion),
    // TODO DollarExp(DollarExp),
    DoubleQuoted(DoubleQuoted),
    Glob(Glob),
    Lit(Lit),
    // TODO ProcSub(ProcSub),
    SingleQuoted(SingleQuoted),
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Lit {
    #[new(into)]
    value: String,
}

impl Lit {
    // TODO: Restrict the access to value?
    pub(crate) fn value(&self) -> &str {
        &self.value
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    ident: String,
    subscripts: Vec<Subscript>,
}

impl Variable {
    pub(crate) fn new(ident: impl Into<String>) -> Self {
        Self::with_subscripts(ident, vec![])
    }

    pub(crate) fn with_subscripts(ident: impl Into<String>, subscripts: Vec<Subscript>) -> Self {
        Self {
            ident: ident.into(),
            subscripts,
        }
    }
}

#[derive(Debug, New, PartialEq)]
pub(crate) struct Subscript {
    // Left unparsed for now.
    #[new(into)]
    content: String,
}
// endregion
