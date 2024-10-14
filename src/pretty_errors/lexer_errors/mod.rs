use std::ops::Range;
use ariadne::{Color, Fmt, Label, Report, ReportBuilder, ReportKind};
use crate::lexer::Error;
use crate::pretty_errors::{PrettyErrorBuilder};

impl PrettyErrorBuilder for Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> ReportBuilder<(&'a str, Range<usize>)> {
        let keyword_color = Color::Fixed(81);
        match self {
            Error::IllegalCharacter { span, found } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Invalid character")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message("this character")
                        .with_color(ariadne::Color::Red))
                    .with_note(format!("'{}' is not a valid character in Mechylang", found))
            }
            Error::UnterminatedString { span } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Unterminated string")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("this {string} is not terminated",
                                              string = "string".fg(keyword_color)))
                        .with_color(ariadne::Color::Red))
                    .with_help("Add a closing quote")
            }
            Error::UnsupportedEscapeSequence { span } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Unsupported escape sequence")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message("this escape sequence is not supported")
                        .with_color(ariadne::Color::Red))
                    .with_help("Mechylang supports the following escape sequences: \\n, \\r, \\t, \\\", and \\\\")
            }
        }
    }
}
