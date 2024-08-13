use crate::{
    parser::Parser,
    TokenKind,
    error::ErrorKind,
    Token
};

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,      // =
    Range,       // .. or ..=
    LogicalOr,   // ||
    LogicalAnd,  // &&
    Equals,      // ==
    LessGreater, // > or <
    BitwiseOr,   // |
    BitwiseXor,  // ^
    BitwiseAnd,  // &
    BitShift,    // << or >>
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -x or !x
    Index,       // array[index]
    Call,        // myFunction(X)
    Member,      // foo.member
}

pub trait PrecedenceTrait {
    fn precedence(&self) -> Option<Precedence>;
}

impl PrecedenceTrait for TokenKind {
    fn precedence(&self) -> Option<Precedence> {
        let precedence = match self {
            // Precedences listed in ascending order, but lowest precedence at the end
            // *** Assign ***
            TokenKind::AssignEqual
            | TokenKind::AssignPlus
            | TokenKind::AssignMinus
            | TokenKind::AssignMultiply
            | TokenKind::AssignDivide
            | TokenKind::AssignModulo
            | TokenKind::AssignBitwiseOr
            | TokenKind::AssignBitwiseAnd
            | TokenKind::AssignBitwiseXor => Precedence::Assign,

            // *** Range ***
            TokenKind::RangeExclusive | TokenKind::RangeInclusive => Precedence::Range,

            // *** Logical Or ***
            TokenKind::LogicalOr => Precedence::LogicalOr,

            // *** Logical And ***
            TokenKind::LogicalAnd => Precedence::LogicalAnd,

            // *** Equals ***
            TokenKind::CompareEqual | TokenKind::CompareNotEqual => Precedence::Equals,

            // *** Less Greater ***
            TokenKind::CompareLess
            | TokenKind::CompareLessEqual
            | TokenKind::CompareGreater
            | TokenKind::CompareGreaterEqual => Precedence::LessGreater,

            // *** Bitwise Or ***
            TokenKind::BitwiseOr => Precedence::BitwiseOr,

            // *** Bitwise Xor ***
            TokenKind::BitwiseXor => Precedence::BitwiseXor,

            // *** Bitwise And ***
            TokenKind::Ampersand => Precedence::BitwiseAnd,

            // *** Bit Shift ***
            TokenKind::BitwiseLeftShift | TokenKind::BitwiseRightShift => Precedence::BitShift,

            // *** Sum ***
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,

            // *** Product ***
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => Precedence::Product,

            // *** Prefix ***
            // does not get inferred from token kind, but is used in parsing prefix expressions

            // *** Index ***
            TokenKind::LeftSquare => Precedence::Index,

            // *** Call ***
            TokenKind::LeftParen => Precedence::Call,

            // *** Member ***
            TokenKind::Dot => Precedence::Member,

            // *** Lowest precedence ***
            // some tokens are not used in expressions
            //
            // But can still show up as a peeked token
            TokenKind::LeftSquirly => Precedence::Lowest,
            TokenKind::RightSquirly => Precedence::Lowest,
            TokenKind::Comma => Precedence::Lowest,
            TokenKind::Return => Precedence::Lowest,
            TokenKind::RightSquare => Precedence::Lowest,
            TokenKind::Identifier(_) => Precedence::Lowest,
            TokenKind::Let => Precedence::Lowest,
            TokenKind::EOF => Precedence::Lowest,
            TokenKind::RightParen => Precedence::Lowest,
            TokenKind::For => Precedence::Lowest,
            TokenKind::In => Precedence::Lowest,
            TokenKind::If => Precedence::Lowest,
            TokenKind::Number(_) => Precedence::Lowest,
            TokenKind::String(_) => Precedence::Lowest,
            TokenKind::Fn => Precedence::Lowest,
            TokenKind::While => Precedence::Lowest,
            TokenKind::Else => Precedence::Lowest,
            TokenKind::True => Precedence::Lowest,
            TokenKind::False => Precedence::Lowest,
            TokenKind::Unit => Precedence::Lowest,
            TokenKind::Break => Precedence::Lowest,
            TokenKind::Continue => Precedence::Lowest,
            _ => {
                return None;
            }
        };

        Some(precedence)
    }
}

impl PrecedenceTrait for Token {
    fn precedence(&self) -> Option<Precedence> {
        self.kind.precedence()
    }
}

impl Parser {
    pub(super) fn peek_precedence(&mut self) -> Precedence {
        match self.peek_token.precedence() {
            Some(precedence) => precedence,
            None => {
                self.errors.push(self.error_peek(
                    ErrorKind::MissingPrecedence,
                    format!("No precedence found for {:?}", self.peek_token),
                ));

                Precedence::Lowest
            }
        }
    }
    
    pub(super) fn cur_precedence(&self) -> Precedence {
        self.cur_token.precedence().unwrap_or(Precedence::Lowest)
    }
}

#[cfg(test)]
mod tests {
    use crate::Lexer;
    use crate::parser::expressions::precedence::Precedence;
    use crate::parser::Parser;

    #[test]
    fn test_precedence_ord() {
        assert!(Precedence::Lowest < Precedence::Assign);
        assert!(Precedence::Assign < Precedence::LogicalOr);
        assert!(Precedence::LogicalOr < Precedence::LogicalAnd);
        assert!(Precedence::LogicalAnd < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::BitwiseOr);
        assert!(Precedence::BitwiseOr < Precedence::BitwiseXor);
        assert!(Precedence::BitwiseXor < Precedence::BitwiseAnd);
        assert!(Precedence::BitShift < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Index);
        assert!(Precedence::Index < Precedence::Call);
        assert!(Precedence::Call < Precedence::Member);
    }


    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b;", "((-a) * b);"),
            ("!-a;", "(!(-a));"),
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            (
                "a + b * c + d / e - f;",
                "(((a + (b * c)) + (d / e)) - f);",
            ),
            ("3 + 4; -5 * 5;", "(3 + 4);\n((-5) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true;", "true;"),
            ("false;", "false;"),
            ("3 > 5 == false;", "((3 > 5) == false);"),
            ("3 < 5 == true;", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
            ("!(true == true);", "(!(true == true));"),
            ("a + add(b * c) + d;", "((a + add((b * c))) + d);"),
            ("1 && 0 == 0;", "(1 && (0 == 0));"),
            ("1 << 1 == 0;", "((1 << 1) == 0);"),
            ("a + b * c;", "(a + (b * c));"),
            ("a == b && c != d;", "((a == b) && (c != d));"),
            ("a | b ^ c & d;", "(a | (b ^ (c & d)));"),
            ("a << b >> c;", "((a << b) >> c);"),
            ("a <= b && c < d || e >= f && g > h;", "(((a <= b) && (c < d)) || ((e >= f) && (g > h)));"),

            // Index expressions
            ("myArray[1 + 1];", "(myArray[(1 + 1)]);"),
            ("myArray[1][2];", "((myArray[1])[2]);"),
            ("a * [1, 2, 3, 4][b * c] * d;", "((a * ([1, 2, 3, 4][(b * c)])) * d);"),
            ("add(a * b[2], b[1], 2 * [1, 2][1]);", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let actual = program.to_string();
            assert_eq!(actual.trim(), expected.trim());
            assert_eq!(parser.errors.len(), 0);
        }
    }
}
