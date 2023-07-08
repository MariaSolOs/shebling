use derive_more::From;

use super::*;

/// Trait for types that represent single tokens, such as keywords
/// and operators.
pub trait Token
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
        pub enum $name {
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

tokenizable! {
    /// A binary operator.
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

/// A binary expression (e.g. `$x + 1`).
#[derive(Debug)]
pub struct BinExpr<O, L, R = L> {
    left: Box<L>,
    right: Box<R>,
    op: O,
}

impl<O, L, R> BinExpr<O, L, R> {
    /// Creates a new binary expression.
    pub fn new(left: impl Into<L>, right: impl Into<R>, op: impl Into<O>) -> Self {
        BinExpr {
            left: Box::new(left.into()),
            right: Box::new(right.into()),
            op: op.into(),
        }
    }
}

tokenizable! {
    /// A unary operator.
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

/// A unary expression (e.g. `++x`).
#[derive(Debug)]
pub struct UnExpr<O, E> {
    expr: Box<E>,
    op: O,
}

impl<O, E> UnExpr<O, E> {
    /// Creates a new unary expression.
    pub fn new(expr: impl Into<E>, op: impl Into<O>) -> Self {
        UnExpr {
            expr: Box::new(expr.into()),
            op: op.into(),
        }
    }
}

/// Arithmetic expressions.
#[derive(Debug, From)]
pub enum ArithTerm {
    /// Binary expression (e.g. `$x + 1`).
    BinExpr(BinExpr<Spanned<BinOp>, ArithTerm>),

    /// Shell expansion.
    Expansion(Vec<WordSgmt>),

    /// Parenthesized expression.
    Group(ArithSeq),

    /// Conditional expression with a ternary operator.
    TriExpr(ArithTriExpr),

    /// Unary expression (e.g. `++x`).
    UnExpr(UnExpr<Spanned<UnOp>, ArithTerm>),
    // TODO: Var(SubscriptedVar),
}

/// Comma-delimited sequence of arithmetic terms.
pub type ArithSeq = Vec<ArithTerm>;

/// An arithmetic ternary expression of the form
/// `condition ? true_branch : else_branch`.
#[derive(Debug)]
pub struct ArithTriExpr {
    cond: Box<ArithTerm>,
    then_branch: Box<ArithTerm>,
    else_branch: Box<ArithTerm>,
}

impl ArithTriExpr {
    /// Creates a new arithmetic ternary expression.
    pub fn new(
        cond: impl Into<ArithTerm>,
        then_branch: impl Into<ArithTerm>,
        else_branch: impl Into<ArithTerm>,
    ) -> Self {
        Self {
            cond: Box::new(cond.into()),
            then_branch: Box::new(then_branch.into()),
            else_branch: Box::new(else_branch.into()),
        }
    }
}
