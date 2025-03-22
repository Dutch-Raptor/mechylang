use crate::parser::Error;
use crate::pretty_errors::{
    highlight_colors, MechyPrettyErrorBuilder, MultiErrorBuilder, PrettyErrorBuilder, KEYWORD_COLOR,
};
use ariadne::{Color, Fmt, Label, Report, ReportKind};

impl PrettyErrorBuilder for Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> MechyPrettyErrorBuilder<'a> {
        let mut color_generator = highlight_colors();
        match self {
            Error::LexerError(err) => err.as_pretty_error_builder(source_id),
            Error::UnterminatedStatement { span } => {
                MechyPrettyErrorBuilder::Single(
                    Report::build(ReportKind::Error, (source_id, span.bytes()))
                        .with_message("Unterminated statement")
                        .with_label(Label::new((source_id, span.bytes.clone()))
                            .with_message("this statement is not terminated")
                            .with_color(Color::Red))
                        .with_help("Add a semicolon to end the statement"))
            }
            Error::UnexpectedToken { span, expected, found, location } => {
                let a = color_generator.next();
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Unexpected token{}", location.as_ref().map_or(String::new(), |loc| format!(" in {}", loc))))
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("Unexpected `{found}' here", found = found.fg(a)))
                        .with_color(a))
                    .with_note(format!(
                        "Expected `{}`, found `{}`",
                        expected.iter().map(|t| t.name().to_string()).collect::<Vec<String>>().join(", "),
                        found
                    )))
            }
            Error::IncompleteReturnStatemen { return_span, .. } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, return_span.bytes()))
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
                    )))
            }
            Error::MissingPrecedence { span, for_token_kind } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Ambiguous order of operations. Missing precedence for {token}",
                                          token = for_token_kind.name()
                    ))
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("this {token} is missing precedence",
                                              token = "token".fg(KEYWORD_COLOR)
                        ))
                        .with_color(Color::Red))
                    .with_help("Add parentheses or brackets to make the expression clearer"))
            }
            Error::InvalidPrefix { span, found } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message("Invalid syntax at start of expression")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("`{value}` cannot be used as the start of an expression.",
                                              value = found.fg(Color::Red)
                        ))
                        .with_color(Color::Red))
                    .with_help("Start expressions with an identifier, literal, operator, control flow keyword, or parenthesis."))
            }
            Error::InvalidNumber { span, .. } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message("Invalid number")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message("this number is not valid")
                        .with_color(Color::Red))
                    .with_help("Check that the number is valid"))
            }
            Error::InvalidInfix { span, found } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message("Invalid infix operator")
                    .with_label(Label::new((source_id, span.bytes.clone()))
                        .with_message(format!("this operator `{operator}` is not valid",
                                              operator = found.fg(Color::Red)
                        ))
                        .with_color(Color::Red))
                    .with_help("Check that the operator is valid"))
            }
            Error::UnexpectedExpressionListEnd { list_span, expected_end_token, found, parse_expression_error } => {
                let unexpected_end_of_expression_list_error =
                    Report::build(ReportKind::Error, (source_id, list_span.bytes()))
                        .with_message("Unexpected end of expression list")
                        .with_label(Label::new((source_id, list_span.bytes.clone()))
                            .with_message(format!("Expected either an expression or `{expected_end_token}`, found `{found}`",
                                                  expected_end_token = expected_end_token.name(),
                                                  found = found.fg(Color::Red),
                            ))
                            .with_color(Color::Red))
                        .with_help(format!(
                            "Check that the expression list ends with a `{expected_end_token}`{}",
                            if parse_expression_error.is_some() {
                                ", or that the expression is valid. See the related error for more details"
                            } else {
                                "."
                            },
                            expected_end_token = expected_end_token.name(),
                        ))
                        .with_note(format!(
                            "Unexpected end of expression list. Expected either an expression or `{expected_end_token}`, found `{found}`",
                            expected_end_token = expected_end_token.name(),
                        ));

                if let Some(parse_expression_error) = parse_expression_error {
                    MechyPrettyErrorBuilder::Multi(MultiErrorBuilder {
                        description: "Unexpected end of expression list.".to_string(),
                        errors: vec![
                            MechyPrettyErrorBuilder::Single(unexpected_end_of_expression_list_error),
                            parse_expression_error.as_pretty_error_builder(source_id),
                        ],
                    })
                } else {
                    MechyPrettyErrorBuilder::Single(unexpected_end_of_expression_list_error)
                }
            }
            Error::InvalidStructKey { span, found } => {
                MechyPrettyErrorBuilder::Single(Report::build(ReportKind::Error, (source_id, span.bytes()))
                    .with_message(format!("Invalid struct key. Expected an identifier, found `{}`", found))
                    .with_label(
                        Label::new((source_id, span.bytes.clone()))
                            .with_message("This key is not a valid identifier".to_string())
                            .with_color(Color::Red)
                            .with_priority(0)
                            .with_order(1),
                    )
                )
            }
        }
    }
}
