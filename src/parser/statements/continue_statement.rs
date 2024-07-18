use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::Token;

#[derive(Debug, PartialEq, Serialize)]
pub struct ContinueStatement {
    pub token: Token,
}

impl Display for ContinueStatement {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{};", self.token)
    }
}
