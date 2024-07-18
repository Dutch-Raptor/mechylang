use std::fmt;
use std::fmt::{Display, Formatter};
use serde::Serialize;
use crate::parser::statements::Statement;

#[derive(Debug, Serialize)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}\n", statement)?;
        }
        Ok(())
    }
}
