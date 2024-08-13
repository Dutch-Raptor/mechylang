use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::{Span};

/// Represents a `continue` statement in Mechylang.
///
/// A `continue` statement is used to skip the remaining part of the current iteration of a loop
/// and proceed to the next iteration.
///
/// # Fields
///
/// * `token` - The token representing the `continue` keyword in the source code.
#[derive(Debug, PartialEq, Serialize)]
pub struct ContinueStatement {
    pub span: Span,
}

impl Display for ContinueStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "continue;")
    }
}
