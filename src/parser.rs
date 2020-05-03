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
        let mut result = vec![self.atomic_expr()?];

        while let Some(stmt) = if self.newline_before() {
            self.try_(|p| p.atomic_expr())?
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
            Ok(Expr::Sequence(result))
        }
    }

    fn atomic_expr(&mut self) -> Result<Expr> {
        self.assignment_expr()
    }

    fn assignment_expr(&mut self) -> Result<Expr> {
        let expr = self.method_call_expr()?;

        match self.peek() {
            Token::Equal => {
                self.next();

                let rhs = self.method_call_expr()?;
                Ok(Expr::Assignment(Box::new(expr), Box::new(rhs)))
            }
            _ => Ok(expr),
        }
    }

    fn method_call_expr(&mut self) -> Result<Expr> {
        let mut expr = self.primary_expr()?;

        while *self.peek() == Token::Dot {
            self.next();
            let method = self.identifier("method call")?;
            let (args, block) = self.method_call_arguments(ListCardinality::PossiblyEmpty)?;
            expr = Expr::MethodCall {
                receiver: Some(Box::new(expr)),
                method,
                args,
                block,
            };
        }
        Ok(expr)
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
    ) -> Result<(Vec<Expr>, Option<Box<Block>>)> {
        if self.newline_before() {
            return self.parse_error("method call arguments", "expression or (");
        }
        let args = match self.peek() {
            Token::LParen => {
                self.next();
                let exprs = self.expr_list(ListCardinality::PossiblyEmpty)?;
                self.consume(Token::RParen, "method call arguments", ")")?;
                exprs
            }
            _ => self.expr_list(cardinality)?,
        };
        let block = match self.peek() {
            Token::Do => {
                self.next();
                let body = self.expr()?;
                self.consume(Token::End, "block", "end")?;
                Some(Box::new(Block {
                    params: vec![],
                    body,
                }))
            }
            _ => None,
        };
        Ok((args, block))
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
                self.consume(Token::RParen, "parenthesized expression", ")")?;
                Ok(expr)
            }
            Token::If => {
                self.next();
                let condition = Box::new(self.expr()?);
                self.consume(Token::Then, "if", "then")?;
                let if_true = Box::new(self.expr()?);
                let if_false = match self.peek() {
                    Token::Else => {
                        self.next();
                        Some(Box::new(self.expr()?))
                    }
                    _ => None,
                };
                self.consume(Token::End, "if", "end")?;
                Ok(Expr::If {
                    condition,
                    if_true,
                    if_false,
                })
            }
            Token::While => {
                self.next();
                let condition = Box::new(self.expr()?);
                self.consume(Token::Do, "while", "do")?;
                let body = Box::new(self.expr()?);
                self.consume(Token::End, "while", "end")?;
                Ok(Expr::While { condition, body })
            }
            _ => self.parse_error(
                "expression",
                "identifier, nil, self, integer literal, string literal, @, :, (",
            ),
        }
    }

    fn consume(&mut self, tok: Token, context: &'static str, expected: &'static str) -> Result<()> {
        if *self.peek() == tok {
            self.next();
            Ok(())
        } else {
            self.parse_error(context, expected)
        }
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
            Err(ParseError::ParseError { pos, .. }) if pos == start_pos => Ok(None),
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
    fn test_chained_method_call() {
        test_parse_expr(
            "foo.bar.baz",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                    method: "bar".to_string(),
                    args: vec![],
                    block: None,
                })),
                method: "baz".to_string(),
                args: vec![],
                block: None,
            }),
        );
    }

    #[test]
    fn test_chained_method_call_with_parens() {
        test_parse_expr(
            "foo.bar(1).baz(2)",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                    method: "bar".to_string(),
                    args: vec![Expr::IntegerLiteral(1)],
                    block: None,
                })),
                method: "baz".to_string(),
                args: vec![Expr::IntegerLiteral(2)],
                block: None,
            }),
        );
    }

    #[test]
    fn test_method_call_inside_arg() {
        test_parse_expr(
            "foo.bar 1.baz",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![Expr::MethodCall {
                    receiver: Some(Box::new(Expr::IntegerLiteral(1))),
                    method: "baz".to_string(),
                    args: vec![],
                    block: None,
                }],
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
        test_parse_expr("foo", Ok(Expr::Var("foo".to_string())));
    }

    #[test]
    fn test_sequence() {
        test_parse_expr(
            "
            foo
            bar
            ",
            Ok(Expr::Sequence(vec![
                Expr::Var("foo".to_string()),
                Expr::Var("bar".to_string()),
            ])),
        );
        test_parse_expr(
            "
            bar()
            foo.baz()
            ",
            Ok(Expr::Sequence(vec![
                Expr::MethodCall {
                    receiver: None,
                    method: "bar".to_string(),
                    args: vec![],
                    block: None,
                },
                Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                    method: "baz".to_string(),
                    args: vec![],
                    block: None,
                },
            ])),
        );
    }

    #[test]
    fn test_sequence_should_require_newline() {
        test_parse_expr(
            "foo.bar() foo.baz()",
            Err(ParseError::ParseError {
                context: "expression",
                expected: "EOF",
                got: Token::Identifier("foo".to_string()),
                pos: 5,
            }),
        );
    }

    #[test]
    fn test_multiline_method_call() {
        test_parse_expr(
            "
            foo.bar 1,
              2,
              3
            ",
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
            "
            foo.bar(
              1,
              2,
              3
            )
            ",
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
    }

    #[test]
    fn test_assignment() {
        test_parse_expr(
            "foo = bar",
            Ok(Expr::Assignment(
                Box::new(Expr::Var("foo".to_string())),
                Box::new(Expr::Var("bar".to_string())),
            )),
        );
    }

    #[test]
    fn test_method_call_has_precedence_over_assignment() {
        test_parse_expr(
            "foo = bar.baz",
            Ok(Expr::Assignment(
                Box::new(Expr::Var("foo".to_string())),
                Box::new(Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("bar".to_string()))),
                    method: "baz".to_string(),
                    args: vec![],
                    block: None,
                }),
            )),
        );
    }

    #[test]
    fn test_if() {
        test_parse_expr(
            "if 1 then 2 end",
            Ok(Expr::If {
                condition: Box::new(Expr::IntegerLiteral(1)),
                if_true: Box::new(Expr::IntegerLiteral(2)),
                if_false: None,
            }),
        );
        test_parse_expr(
            "if 1 then 2 else 3 end",
            Ok(Expr::If {
                condition: Box::new(Expr::IntegerLiteral(1)),
                if_true: Box::new(Expr::IntegerLiteral(2)),
                if_false: Some(Box::new(Expr::IntegerLiteral(3))),
            }),
        );
    }

    #[test]
    fn test_while() {
        test_parse_expr(
            "while 1 do 2 end",
            Ok(Expr::While {
                condition: Box::new(Expr::IntegerLiteral(1)),
                body: Box::new(Expr::IntegerLiteral(2)),
            }),
        );
    }

    #[test]
    fn test_method_call_on_receiver_with_block() {
        test_parse_expr(
            "foo.bar do baz end",
            Ok(Expr::MethodCall {
                receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                method: "bar".to_string(),
                args: vec![],
                block: Some(Box::new(Block {
                    params: vec![],
                    body: Expr::Var("baz".to_string()),
                })),
            }),
        );
    }

    #[test]
    #[ignore = "Bug: We parse this wrong - block is consumed by method call, not by while."]
    fn test_while_with_method_call() {
        test_parse_expr(
            "while foo.bar do baz end",
            Ok(Expr::While {
                condition: Box::new(Expr::MethodCall {
                    receiver: Some(Box::new(Expr::Var("foo".to_string()))),
                    method: "bar".to_string(),
                    args: vec![],
                    block: None,
                }),
                body: Box::new(Expr::Var("baz".to_string())),
            }),
        );
    }
}
