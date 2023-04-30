use super::*;

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
         enum $name {
             $(
                 /// ```sh
                 #[doc = $token]
                 /// ```
                 ///
                 $(#[doc = $arm_doc])*
                 $arm
             ),+
         }

         impl $crate::token::Token for $name {
             fn token(&self) -> &'static str {
                 match self {
                     $(Self::$arm => $token),+
                 }
             }
         }
     };
 }

/// Trait for [Token] types which can be directly parsed from a [Span].
pub(crate) trait ParseToken
where
    Self: Token,
{
    fn parse_token<'a>(self, span: Span<'a>) -> ParseResult<'a, Self> {
        // This isn't recursing, it calls the generic function below.
        // By having a separate inner function, override implementations
        // can still use the default `parse_token`.
        parse_token(self)(span)
    }
}

/// Creates a parser for the given token based on its `token()` pattern.
fn parse_token<'a, T: Token>(token: T) -> impl FnMut(Span<'a>) -> ParseResult<T> {
    value(token, tag(token.token()))
}

impl ParseToken for BinOp {}

impl ParseToken for UnOp {}
