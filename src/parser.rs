use crate::ast::*;
use crate::lexer;
use crate::token::{Token, TokenInfo};

#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    ParseError {
        context: &'static str,
        expected: &'static str,
        got: Token,
        pos: usize, // token index, not character index
    },
    LexicalError(String),
}

impl From<lexer::Error> for ParseError {
    fn from(e: lexer::Error) -> Self {
        ParseError::LexicalError(e.0)
    }
}

type Result<T> = ::std::result::Result<T, ParseError>;

pub fn parse_expr(input: &str) -> Result<Expr> {
    let tokens = lexer::lex(input)?;
    let mut parser = Parser::from_tokens(&tokens);
    let result = parser.expr()?;
    if !parser.eof() {
        parser.parse_error("expression", "EOF")
    } else {
        Ok(result)
    }
}

pub fn parse_stmt(input: &str) -> Result<Statement> {
    let tokens = lexer::lex(input)?;
    let mut parser = Parser::from_tokens(&tokens);
    let result = parser.stmt()?;
    if !parser.eof() {
        parser.parse_error("statement", "EOF")
    } else {
        Ok(result)
    }
}

struct Parser<'a> {
    input: &'a [TokenInfo],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn from_tokens(tokens: &'a [TokenInfo]) -> Self {
        Parser {
            input: tokens,
            pos: 0,
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        let expr = self.primary_expr()?;

        match self.peek() {
            Token::Dot => {
                self.next();
                let method = self.identifier("method call")?;
                let (args, block) = self.method_call_arguments(ListCardinality::PossiblyEmpty)?;
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

    fn method_call_arguments(
        &mut self,
        cardinality: ListCardinality,
    ) -> Result<(Vec<Expr>, Option<Block>)> {
        if self.newline_before() {
            return self.parse_error("method call arguments", "expression or (");
        }
        let args = match self.peek() {
            Token::LParen => {
                self.next();
                let exprs = self.expr_list(ListCardinality::PossiblyEmpty)?;
                match self.peek() {
                    Token::RParen => {
                        self.next();
                        exprs
                    }
                    _ => {
                        return self.parse_error("method call arguments", ")");
                    }
                }
            }
            _ => self.expr_list(cardinality)?,
        };
        Ok((args, None))
    }

    fn expr_list(&mut self, cardinality: ListCardinality) -> Result<Vec<Expr>> {
        let mut result = vec![];
        match self.try_(|p| p.expr())? {
            Some(expr) => result.push(expr),
            None => match cardinality {
                ListCardinality::PossiblyEmpty => return Ok(vec![]),
                ListCardinality::NonEmpty => {
                    return self.parse_error("expression list", "expression")
                }
            },
        }
        while *self.peek() == Token::Comma {
            self.next();
            result.push(self.expr()?);
        }
        Ok(result)
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

                if let Some((args, block)) =
                    self.try_(|p| p.method_call_arguments(ListCardinality::NonEmpty))?
                {
                    return Ok(Expr::MethodCall {
                        receiver: None,
                        method: ident,
                        args,
                        block,
                    });
                }

                Ok(Expr::Var(ident))
            }
            Token::LParen => {
                self.next();
                let expr = self.expr()?;
                match self.peek() {
                    Token::RParen => {
                        self.next();
                        Ok(expr)
                    }
                    _ => self.parse_error("parenthesized expression", ")"),
                }
            }
            _ => self.parse_error(
                "expression",
                "identifier, nil, self, integer literal, string literal, @, :, (",
            ),
        }
    }

    fn stmt(&mut self) -> Result<Statement> {
        let mut result = vec![self.atomic_stmt()?];

        while let Some(stmt) = if self.newline_before() {
            self.try_(|p| p.atomic_stmt())?
        } else {
            None
        } {
            result.push(stmt);
        }

        if result.len() == 1 {
            Ok(result
                .pop()
                .expect("we just checked that the vector has len 1"))
        } else {
            Ok(Statement::Sequence(result))
        }
    }

    fn atomic_stmt(&mut self) -> Result<Statement> {
        Ok(Statement::Expression(self.expr()?))
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek(&self) -> &Token {
        if self.eof() {
            &Token::EOF
        } else {
            &self.input[self.pos].token
        }
    }

    fn newline_before(&self) -> bool {
        if self.pos >= self.input.len() {
            false
        } else {
            self.input[self.pos].newline_before
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
            pos: self.pos,
        })
    }

    fn try_<A, F>(&mut self, parse: F) -> Result<Option<A>>
    where
        F: FnOnce(&mut Self) -> Result<A>,
    {
        let start_pos = self.pos;
        match parse(self) {
            Err(ParseError::ParseError {
                context: _,
                expected: _,
                got: _,
                pos,
            }) if pos == start_pos => Ok(None),
            r => r.map(Some),
        }
    }
}

enum ListCardinality {
    NonEmpty,
    PossiblyEmpty,
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

    fn test_parse_stmt(input: &str, expected_result: Result<Statement>) {
        let result = parse_stmt(input);
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
                pos: 1,
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
                pos: 1,
            }),
        );
    }

    #[test]
    fn test_parens() {
        test_parse_expr("(foo)", Ok(Expr::Var("foo".to_string())));
        test_parse_expr("(((foo)))", Ok(Expr::Var("foo".to_string())));
    }

    #[test]
    fn test_method_call_on_receiver() {
        test_parse_expr(
            "foo.bar",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![],
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
                args: vec![Expr::IntegerLiteral(1)],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar()",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar(1, 2)",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1), Expr::IntegerLiteral(2)],
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
                ],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar(1,",
            Err(ParseError::ParseError {
                context: "expression",
                expected: "identifier, nil, self, integer literal, string literal, @, :, (",
                got: Token::EOF,
                pos: 6,
            }),
        );
        test_parse_expr(
            "foo.bar(1,2",
            Err(ParseError::ParseError {
                context: "method call arguments",
                expected: ")",
                got: Token::EOF,
                pos: 7,
            }),
        );
    }

    #[test]
    fn test_method_call_on_receiver_without_parens() {
        test_parse_expr(
            "foo.bar 1",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1)],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar 1, 2 ",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1), Expr::IntegerLiteral(2)],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar 1, 2, 3 ",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ],
                block: None,
            }),
        );
        test_parse_expr(
            "foo.bar 1,",
            Err(ParseError::ParseError {
                context: "expression",
                expected: "identifier, nil, self, integer literal, string literal, @, :, (",
                got: Token::EOF,
                pos: 5,
            }),
        );
    }

    #[test]
    fn test_method_call_on_self_without_parens() {
        test_parse_expr(
            "bar 1",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1)],
                block: None,
            }),
        );
        test_parse_expr(
            "bar 1, 2 ",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1), Expr::IntegerLiteral(2)],
                block: None,
            }),
        );
        test_parse_expr(
            "bar 1, 2, 3 ",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ],
                block: None,
            }),
        );
        test_parse_expr(
            "bar 1,",
            Err(ParseError::ParseError {
                context: "expression",
                expected: "identifier, nil, self, integer literal, string literal, @, :, (",
                got: Token::EOF,
                pos: 3,
            }),
        );
    }

    #[test]
    fn test_method_call_on_self_with_parens() {
        test_parse_expr(
            "bar(1)",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1)],
                block: None,
            }),
        );
        test_parse_expr(
            "bar()",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![],
                block: None,
            }),
        );
        test_parse_expr(
            "bar(1, 2)",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![Expr::IntegerLiteral(1), Expr::IntegerLiteral(2)],
                block: None,
            }),
        );
        test_parse_expr(
            "bar(1, 2, 3)",
            Ok(Expr::MethodCall {
                receiver: None,
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ],
                block: None,
            }),
        );
        test_parse_expr(
            "bar(1,",
            Err(ParseError::ParseError {
                context: "expression",
                expected: "identifier, nil, self, integer literal, string literal, @, :, (",
                got: Token::EOF,
                pos: 4,
            }),
        );
        test_parse_expr(
            "bar(1,2",
            Err(ParseError::ParseError {
                context: "method call arguments",
                expected: ")",
                got: Token::EOF,
                pos: 5,
            }),
        );
    }

    #[test]
    fn test_single_stmt() {
        test_parse_stmt(
            "foo",
            Ok(Statement::Expression(Expr::Var("foo".to_string()))),
        );
    }

    #[test]
    fn test_sequence() {
        test_parse_stmt(
            "
            foo
            bar
            ",
            Ok(Statement::Sequence(vec![
                Statement::Expression(Expr::Var("foo".to_string())),
                Statement::Expression(Expr::Var("bar".to_string())),
            ])),
        );
        test_parse_stmt(
            "
            bar()
            foo.baz()
            ",
            Ok(Statement::Sequence(vec![
                Statement::Expression(Expr::MethodCall {
                    receiver: None,
                    method: "bar".to_string(),
                    args: vec![],
                    block: None,
                }),
                Statement::Expression(Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                    method: "baz".to_string(),
                    args: vec![],
                    block: None,
                }),
            ])),
        );
    }

    #[test]
    fn test_sequence_should_require_newline() {
        test_parse_stmt(
            "foo.bar() foo.baz()",
            Err(ParseError::ParseError {
                context: "statement",
                expected: "EOF",
                got: Token::Identifier("foo".to_string()),
                pos: 5,
            }),
        );
    }

    #[test]
    fn test_multiline_method_call() {
        test_parse_stmt(
            "
            foo.bar 1,
              2,
              3
            ",
            Ok(Statement::Expression(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ],
                block: None,
            })),
        );
        test_parse_stmt(
            "
            foo.bar(
              1,
              2,
              3
            )
            ",
            Ok(Statement::Expression(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![
                    Expr::IntegerLiteral(1),
                    Expr::IntegerLiteral(2),
                    Expr::IntegerLiteral(3),
                ],
                block: None,
            })),
        );
    }
}
