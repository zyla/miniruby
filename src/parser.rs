use crate::ast::*;
use crate::lexer;
use crate::token::{Token, TokenInfo};

use PartialResult::*;

#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    ParseError {
        context: &'static str,
        expected: &'static str,
        got: Token,
    },
    LexicalError(String),
}

impl From<lexer::Error> for ParseError {
    fn from(e: lexer::Error) -> Self {
        ParseError::LexicalError(e.0)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum PartialResult<T> {
    Ok(T),
    Err(ParseError),
    NoMatch,
}

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let tokens = lexer::lex(input)?;
    let mut parser = Parser {
        input: &tokens,
        pos: 0,
    };
    match parser.expr() {
        Ok(x) => Result::Ok(x),
        NoMatch => Result::Err(ParseError::ParseError {
            context: "expression",
            expected: "expression",
            got: parser.peek().clone(),
        }),
        Err(e) => Result::Err(e),
    }
}

struct Parser<'a> {
    input: &'a [TokenInfo],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn expr(&mut self) -> PartialResult<Expr> {
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> PartialResult<Expr> {
        match self.peek() {
            Token::Nil => {
                self.next();
                Ok(Expr::NilLiteral)
            }
            Token::Self_ => {
                self.next();
                Ok(Expr::SelfLiteral)
            }
            Token::IntegerLiteral(value) => {
                let value = *value;
                self.next();
                Ok(Expr::IntegerLiteral(value))
            }
            Token::StringLiteral(value) => {
                let value = value.clone();
                self.next();
                Ok(Expr::StringLiteral(value))
            }
            Token::Colon => {
                self.next();
                match self.peek() {
                    Token::Identifier(s) => {
                        let s = s.clone();
                        self.next();
                        Ok(Expr::Symbol(s))
                    }
                    Token::StringLiteral(s) => {
                        let s = s.clone();
                        self.next();
                        Ok(Expr::Symbol(s))
                    }
                    _ => self.parse_error("symbol", "identifier, string literal"),
                }
            }
            Token::At => {
                self.next();
                match self.peek() {
                    Token::Identifier(s) => {
                        let s = s.clone();
                        self.next();
                        Ok(Expr::InstanceVariable(s))
                    }
                    _ => self.parse_error("instance variable", "identifier"),
                }
            }
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.next();
                Ok(Expr::Var(ident))
            }
            _ => NoMatch,
        }
    }

    fn peek(&self) -> &Token {
        if self.pos >= self.input.len() {
            &Token::EOF
        } else {
            &self.input[self.pos].token
        }
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    fn parse_error<T>(&self, context: &'static str, expected: &'static str) -> PartialResult<T> {
        Err(ParseError::ParseError {
            context,
            expected,
            got: self.peek().clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::result::Result::{Err, Ok};

    fn test_parse_expr(input: &str, expected_result: Result<Expr, ParseError>) {
        let result = parse_expr(input);
        if result != expected_result {
            panic!("Test failed.\n            input: {:?}\n  expected result: {:?}\n    actual result: {:?}\n",
                   input, expected_result, result);
        }
    }

    #[test]
    fn test_literals() {
        test_parse_expr("nil", Ok(Expr::NilLiteral));
        test_parse_expr("self", Ok(Expr::SelfLiteral));
        test_parse_expr("123", Ok(Expr::IntegerLiteral(123)));
        test_parse_expr(r#"  "foo"  "#, Ok(Expr::StringLiteral("foo".to_string())));
        test_parse_expr(":foo", Ok(Expr::Symbol("foo".to_string())));
        test_parse_expr(r#"  :"foo"  "#, Ok(Expr::Symbol("foo".to_string())));
        test_parse_expr(
            ":1",
            Err(ParseError::ParseError {
                context: "symbol",
                expected: "identifier, string literal",
                got: Token::IntegerLiteral(1),
            }),
        );
    }

    #[test]
    fn test_variables() {
        test_parse_expr("foo", Ok(Expr::Var("foo".to_string())));
        test_parse_expr("@foo", Ok(Expr::InstanceVariable("foo".to_string())));
        test_parse_expr(
            "@1",
            Err(ParseError::ParseError {
                context: "instance variable",
                expected: "identifier",
                got: Token::IntegerLiteral(1),
            }),
        );
    }
}
