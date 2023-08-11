use crate::lexer::lexer::Lexer;
use crate::lexer::tokens::TokenKind;

pub struct Compiler<T: AssemblyOutput> {
    lexer: Lexer,
    output: T,
}

impl<T: AssemblyOutput> Compiler<T> {
    pub fn new(source: String, output: T) -> Compiler<T> {
        Compiler {
            lexer: Lexer::new(source),
            output,
        }
    }
}

pub trait AssemblyOutput {
    fn mov(&self, source: &str, destination: &str) -> String;
    fn add(&self, source: &str, destination: &str) -> String;
    fn sub(&self, source: &str, destination: &str) -> String;
    fn mul(&self, source: &str, destination: &str) -> String;
    fn div(&self, source: &str, destination: &str) -> String;
}

pub(crate) struct X86_64Intel;

impl AssemblyOutput for X86_64Intel {
    /// mov destination, source
    fn mov(&self, source: &str, destination: &str) -> String {
        format!("mov {}, {}", destination, source)
    }

    fn add(&self, source: &str, destination: &str) -> String {
        todo!()
    }

    fn sub(&self, source: &str, destination: &str) -> String {
        todo!()
    }

    fn mul(&self, source: &str, destination: &str) -> String {
        todo!()
    }

    fn div(&self, source: &str, destination: &str) -> String {
        todo!()
    }
}
