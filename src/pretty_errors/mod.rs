use std::ops::Range;
use ariadne::{Color, Config, LabelAttach, Report, ReportBuilder};

mod lexer_errors;
mod parser_errors;
mod evaluator_errors;

pub trait PrettyError {
    fn as_pretty_error<'a>(&self, source_id: &'a str) -> Report<(&'a str, Range<usize>)>;
}

struct HighlightColors {
    colors: [Color; 3],
    index: usize,
    color_generator: ariadne::ColorGenerator,
}

impl HighlightColors {
    fn new() -> Self {
        Self {
            // Credit https://github.com/zkat/miette/blob/5f441d011560a091fe5d6a6cdb05f09acf622d36/src/handlers/theme.rs#L112
            colors: [
                Color::Rgb(246, 87, 248),
                Color::Rgb(30, 201, 212),
                Color::Rgb(145, 246, 111),
            ],
            index: 0,
            color_generator: ariadne::ColorGenerator::new(),
        }
    }
    
    fn next(&mut self) -> Color {
        if self.index >= self.colors.len() {
            self.color_generator.next()
        } else {
            let color = self.colors[self.index];
            self.index += 1;
            color
        }
    }
}

fn highlight_colors() -> HighlightColors {
    HighlightColors::new()
}

const KEYWORD_COLOR: Color = Color::Fixed(81);

impl<T:PrettyErrorBuilder> PrettyError for T {
    fn as_pretty_error<'a>(&self, source_id: &'a str) -> Report<(&'a str, Range<usize>)> {
        self.as_pretty_error_builder(source_id)
            .with_config(Config::default()
                .with_index_type(ariadne::IndexType::Byte)
                .with_label_attach(LabelAttach::Start)
            )
            .finish()
    }
}

trait PrettyErrorBuilder {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> ReportBuilder<(&'a str, Range<usize>)>;
}


impl PrettyErrorBuilder for crate::Error {
    fn as_pretty_error_builder<'a>(&self, source_id: &'a str) -> ReportBuilder<(&'a str, Range<usize>)> {
        match self {
            crate::Error::LexerError(err) => err.as_pretty_error_builder(source_id),
            crate::Error::ParserError(err) => err.as_pretty_error_builder(source_id),
            crate::Error::EvaluatorError(err) => err.as_pretty_error_builder(source_id),
        }
    }
}
