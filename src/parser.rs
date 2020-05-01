use crate::ast::*;
use crate::lexer;
use crate::token::{Token, TokenInfo};

#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    ParseError {
        context: &'static str,
        expected: &'static str,
        got: Token,
    },
    LexicalError(String),
    NoMatch,
}

use ParseError::NoMatch;

impl From<lexer::Error> for ParseError {
    fn from(e: lexer::Error) -> Self {
        ParseError::LexicalError(e.0)
    }
}

type Result<T> = ::std::result::Result<T, ParseError>;

pub fn parse_expr(input: &str) -> Result<Expr> {
    let tokens = lexer::lex(input)?;
    let mut parser = Parser {
        input: &tokens,
        pos: 0,
    };
    match parser.expr() {
        Ok(x) => Ok(x),
        Err(NoMatch) => Err(ParseError::ParseError {
            context: "expression",
            expected: "expression",
            got: parser.peek().clone(),
        }),
        Err(e) => Err(e),
    }
}

struct Parser<'a> {
    input: &'a [TokenInfo],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn expr(&mut self) -> Result<Expr> {
        let expr = self.primary_expr()?;

        match self.peek() {
            Token::Dot => {
                self.next();
                let method = self.identifier("method call")?;
                let (args, block) = self.method_call_arguments()?;
                Ok(Expr::MethodCall {
                    receiver: Some(Box::new(expr)),
                    method,
                    args,
                    block,
                })
            }
            _ => Ok(expr),
        }
    }

    fn identifier(&mut self, context: &'static str) -> Result<Identifier> {
        match self.peek() {
            Token::Identifier(s) => {
                let s = s.clone();
                self.next();
                Ok(s)
            }
            _ => self.parse_error(context, "identifier"),
        }
    }

    fn method_call_arguments(&mut self) -> Result<(Box<[Expr]>, Option<Block>)> {
        let args = match self.peek() {
            Token::LParen => {
                self.next();
                self.expr_list()?
            }
            _ => vec![].into(),
        };
        Ok((args, None))
    }

    fn expr_list(&mut self) -> Result<Box<[Expr]>> {
        let mut result = vec![];
        match try_(self.expr())? {
            Some(expr) => result.push(expr),
            None => return Ok(vec![].into()),
        }
        while *self.peek() == Token::Comma {
            self.next();
            result.push(self.expr()?);
        }
        Ok(result.into())
    }

    fn primary_expr(&mut self) -> Result<Expr> {
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
                let ident = self.identifier("instance variable")?;
                Ok(Expr::InstanceVariable(ident))
            }
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.next();
                Ok(Expr::Var(ident))
            }
            _ => Err(NoMatch),
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

    fn parse_error<T>(&self, context: &'static str, expected: &'static str) -> Result<T> {
        Err(ParseError::ParseError {
            context,
            expected,
            got: self.peek().clone(),
        })
    }
}

fn try_<A>(r: Result<A>) -> Result<Option<A>> {
    match r {
        Err(NoMatch) => Ok(None),
        _ => r.map(Some),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_parse_expr(input: &str, expected_result: Result<Expr>) {
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

    #[test]
    fn test_method_call_on_receiver() {
        test_parse_expr(
            "foo.bar",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![].into_boxed_slice(),
                block: None,
            }),
        );
    }

    #[test]
    fn test_method_call_on_receiver_with_parens() {
        test_parse_expr(
            "foo.bar(1)",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1)].into_boxed_slice(),
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar(1, 2)",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1), Expr::IntegerLiteral(2)].into_boxed_slice(),
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar(1, 2, 3)",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ]
                .into_boxed_slice(),
                block: None,
            }),
        );
    }
}
