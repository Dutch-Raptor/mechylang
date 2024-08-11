use crate::{Error, Token, trace};
use crate::error::ErrorKind;
use crate::parser::Parser;

impl Parser {
    pub(crate) fn error(
        &self,
        kind: ErrorKind,
        msg: impl ToString,
        token: Option<&Token>,
        context: Option<String>,
    ) -> Error {
        let error = Error::new(kind, msg, token, &self.lines, context);
        trace!(format!("Error: {:?}", error).as_str());
        error
    }

    pub(crate) fn error_current(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(kind, msg, Some(&self.cur_token), None)
    }

    pub(crate) fn error_current_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.cur_token),
            Some(context),
        )
    }

    pub(crate) fn error_peek(&self, kind: ErrorKind, msg: impl ToString) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.peek_token),
            None,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn error_peek_with_context(&self, kind: ErrorKind, msg: impl ToString, context: String) -> Error {
        self.error(
            kind,
            msg,
            Some(&self.peek_token),
            Some(context),
        )
    }
}