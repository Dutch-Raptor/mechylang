use crate::{Error, Evaluator, Span, trace};
use crate::error::ErrorKind;

impl Evaluator {
    pub(super) fn error(&self, span: Span, message: &str, error_kind: ErrorKind) -> Error {
        let _trace =
            trace!(&format!("error({:?}, {}, {:?})", span, message, error_kind).to_string());

        Error::new(error_kind, message, span, &self.lines, None)
    }
}