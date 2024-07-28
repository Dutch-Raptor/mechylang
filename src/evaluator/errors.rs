use crate::{Error, Evaluator, Token, trace};
use crate::errors::ErrorKind;

impl Evaluator {
    pub(super) fn error(&self, token: Option<&Token>, message: &str, error_kind: ErrorKind) -> Error {
        let _trace =
            trace!(&format!("error({:?}, {}, {:?})", token, message, error_kind).to_string());

        Error::new(error_kind, message, token, &self.lines, None)
    }
}