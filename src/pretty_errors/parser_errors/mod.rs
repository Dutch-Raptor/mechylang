use std::ops::Range;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind};
use crate::parser::Error;
use crate::pretty_errors::{highlight_colors, PrettyErrorBuilder, KEYWORD_COLOR};

impl PrettyErrorBuilder for Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> ReportBuilder<(&'a str, Range<usize>)> {
        let mut color_generator = highlight_colors();
        match self {
            Error::LexerError(err) => err.as_pretty_error_builder(source_id),
            Error::UnterminatedStatement { span } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Unterminated statement")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message("this statement is not terminated")
                        .with_color(Color::Red))
                    .with_help("Add a semicolon to end the statement")
            }
            Error::UnexpectedToken { span, expected, found, location } => {
                let a = color_generator.next();
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message(format!("Unexpected token{}", location.as_ref().map_or(String::new(), |loc| format!(" in {}", loc))))
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("Unexpected `{found}' here", found = found.fg(a)))
                        .with_color(a))
                    .with_note(format!(
                        "Expected `{}`, found `{}`",
                        expected.iter().map(|t| t.name().to_string()).collect::<Vec<String>>().join(", "),
                        found
                    ))
            }
            Error::IncompleteReturnStatemen { return_span, .. } => {
                Report::build(ReportKind::Error, source_id, return_span.start())
                    .with_message(format!("Incomplete {return} statement.",
                        return = "return".fg(KEYWORD_COLOR)
                    ))
                    .with_label(Label::new((source_id, return_span.bytes.clone()))
                        .with_message(format!("Expected a value or semicolon after {return}",
                            return = "return".fg(KEYWORD_COLOR)
                        ))
                        .with_color(KEYWORD_COLOR))
                    .with_help(format!("Add a {return} value or semicolon (`{return};') to complete the statement",
                        return = "return".fg(KEYWORD_COLOR)
                    ))
            }
            Error::MissingPrecedence { span, for_token_kind } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message(format!("Ambiguous order of operations. Missing precedence for {token}",
                        token = for_token_kind.name()
                    ))
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("this {token} is missing precedence",
                            token = "token".fg(KEYWORD_COLOR)
                        ))
                        .with_color(Color::Red))
                    .with_help("Add parentheses or brackets to make the expression clearer")
            }
            Error::InvalidPrefix { span, found } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Invalid syntax at start of expression.")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("`{value}` cannot be used as the start of an expression.",
                            value = found.fg(Color::Red)
                        ))
                        .with_color(Color::Red))
                    .with_help("Start expressions with an identifier, literal, operator, control flow keyword, or parenthesis.")
            }
            Error::InvalidNumber { span, .. } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Invalid number")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message("this number is not valid")
                        .with_color(Color::Red))
                    .with_help("Check that the number is valid")
            }
            Error::InvalidInfix { span, found } => {
                Report::build(ReportKind::Error, source_id, span.start())
                    .with_message("Invalid infix operator")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("this operator `{operator}` is not valid",
                            operator = found.fg(Color::Red)
                        ))
                        .with_color(Color::Red))
                    .with_help("Check that the operator is valid")
            }
            _ => Report::build(ReportKind::Error, source_id, 0).with_message("Unknown parser error")
        }
    }
}
//
//             Error::UnterminatedStatement { span } => {
//                 miette::miette!(
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this statement".to_string()), span.start(), span.length())
//                     ],
//                     help = "Add a semicolon to end the statement",
//                     "Unterminated statement",
//                 )
//             }
//             Error::UnexpectedToken { span, expected, found } => {
//                 miette::miette!(
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("here".to_string()), span.start(), span.length())
//                     ],
//                     help = "Try adding the missing token",
//                     "Unexpected token. Expected {expected}, found {}",
//                     found,
//                     expected = if expected.len() == 1 {
//                         expected[0].name().to_string()
//                     } else {
//                         format!("one of {}", expected.iter().map(|t| t.name().to_string()).collect::<Vec<String>>().join(", "))
//                     },
//                 )
//             }
//             Error::AmbiguousReturn { return_span, found } => {
//                 miette::miette!(
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this return statement".to_string()), return_span.start(), return_span.length())
//                     ],
//                     help = "Add an expression or semicolon after the return keyword",
//                     "Expected expression or semicolon after return keyword. Found {}",
//                     found,
//                 )
//             }
//             Error::UnexpectedExpressionListEnd { list_span, expected_end_token, found, parse_expression_error } => {
//                 let unexpected_end_of_expression_list_error = miette::miette!(
//
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("Here".to_string()), list_span.start(), list_span.length())
//                     ],
//                     help = format!(
//                         "Check that the expression list ends with a `{expected_end_token}`{}",
//                         if parse_expression_error.is_some() {
//                             ", or that the expression is valid. See the related error for more details"
//                         } else {
//                             "."
//                         },
//                         expected_end_token = expected_end_token.name(),
//                     ),
//                     "Unexpected end of expression list. Expected either an expression or `{expected_end_token}`, found `{found}`",
//                     expected_end_token = expected_end_token.name(),
//                 );
//
//                 if let Some(parse_expression_error) = parse_expression_error {
//                     MultiError {
//                         error: "Unexpected end of expression list.".to_string(),
//                         errors: vec![
//                             unexpected_end_of_expression_list_error,
//                             parse_expression_error.as_pretty_error(),
//                         ],
//                     }
//                         .into()
//                 } else {
//                     unexpected_end_of_expression_list_error
//                 }
//             }
//             Error::MissingPrecedence { span, for_token_kind } => {
//                 miette::miette!(
//                     code = "parser::MissingPrecedence",
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this expression".to_string()), span.start(), span.length())
//                     ],
//                     help = "Add parentheses or brackets to make the expression clearer",
//                     "Missing precedence for token `{}`. Expected an expression with higher precedence",
//                     for_token_kind.name(),
//                 )
//             }
//             Error::InvalidPrefix { span, found } => {
//                 miette::miette!(
//                     code = "parser::InvalidPrefix",
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("Here".to_string()), span.start(), span.length())
//                     ],
//                     "`{}` is not valid as the start of an expression",
//                     found,
//                 )
//             }
//             Error::InvalidNumber { span, found } => {
//                 miette::miette!(
//                     code = "parser::InvalidNumber",
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this token".to_string()), span.start(), span.length())
//                     ],
//                     help = "Check that the number is valid",
//                     "`{}` could not be parsed as a number",
//                     found,
//                 )
//             }
//             Error::InvalidInfix { span, found } => {
//                 miette::miette!(
//                     code = "parser::InvalidInfix",
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this token".to_string()), span.start(), span.length())
//                     ],
//                     "`{}` is not a valid infix operator",
//                     found,
//                 )
//             }
//             Error::InvalidStructKey { span, found } => {
//                 miette::miette!(
//                     code = "parser::InvalidStructKey",
//                     labels = vec![
//                         miette::LabeledSpan::new(Some("this token".to_string()), span.start(), span.length())
//                     ],
//                     help = "Struct keys can only be strings",
//                     "`{}` is not a valid struct key",
//                     found,
//                 )
//             }
//         }
//     }
// }
