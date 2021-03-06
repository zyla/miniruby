use crate::token::{Token, TokenInfo};

pub type Result<A> = ::std::result::Result<A, Error>;

#[derive(PartialEq, Eq, Debug)]
pub struct Error(pub String);

pub fn lex(input: &str) -> Result<Vec<TokenInfo>> {
    let mut l = Lexer {
        input: input.as_bytes(),
        pos: 0,
        tokens: vec![],
        token_start: 0,
        has_newline: false,
    };
    l.lex()?;
    Ok(l.tokens)
}

struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    tokens: Vec<TokenInfo>,
    token_start: usize,
    has_newline: bool,
}

impl<'a> Lexer<'a> {
    fn lex(&mut self) -> Result<()> {
        while !self.eof() {
            // Skip spaces; record if we encountered a newline.
            self.has_newline = false;
            loop {
                if self.eof() {
                    return Ok(());
                }
                let c = self.peek();
                self.has_newline = self.has_newline || c == '\n';
                match c {
                    // Eat a comment
                    '#' => {
                        while !self.eof() && self.peek() != '\n' {
                            self.next();
                        }
                    }
                    c if !c.is_whitespace() => {
                        break;
                    }
                    _ => {}
                }
                self.next();
            }

            let c = self.peek();
            self.token_start = self.pos;
            self.next();

            // Determine token type
            match c {
                c if is_ident_start(c) => {
                    while !self.eof() && is_ident_char(self.peek()) {
                        self.next();
                    }
                    self.push_token(ident_to_token(&self.input[self.token_start..self.pos]))
                }
                c if is_digit(c) => {
                    let mut value = digit_value(c);
                    while !self.eof() && is_integer_literal_char(self.peek()) {
                        if is_digit(self.peek()) {
                            value = value * 10 + digit_value(self.peek());
                        }
                        self.next();
                    }
                    self.push_token(Token::IntegerLiteral(value));
                }
                '"' => {
                    let s = self.parse_string_literal()?;
                    self.push_token(Token::StringLiteral(s));
                }
                ':' => match self.peek() {
                    '"' => {
                        self.next();
                        let s = self.parse_string_literal()?;
                        self.push_token(Token::Symbol(s));
                    }
                    _ => {
                        while !self.eof() && is_symbol_char(self.peek()) {
                            self.next();
                        }
                        self.push_token(Token::Symbol(
                            String::from_utf8(
                                self.input[(self.token_start + 1)..self.pos].to_vec(),
                            )
                            .unwrap(),
                        ));
                    }
                },
                '(' => self.push_token(Token::LParen),
                ')' => self.push_token(Token::RParen),
                '=' => self.push_token(Token::Equal),
                '|' => self.push_token(Token::Pipe),
                '@' => self.push_token(Token::At),
                ',' => self.push_token(Token::Comma),
                ';' => self.push_token(Token::Semicolon),
                '.' => self.push_token(Token::Dot),
                _ => {
                    return Err(Error(format!("Unknown character: {}", c)));
                }
            }
        }
        Ok(())
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
    fn peek(&self) -> char {
        self.input[self.pos] as char
    }
    fn next(&mut self) {
        self.pos += 1;
    }
    fn push_token(&mut self, token: Token) {
        self.tokens.push(TokenInfo {
            token,
            start: self.token_start,
            end: self.pos,
            newline_before: self.has_newline,
        });
    }

    fn parse_string_literal(&mut self) -> Result<String> {
        let mut content: Vec<u8> = vec![];
        loop {
            if self.eof() {
                return Err(Error("Unterminated string literal".to_string()));
            }
            match self.peek() {
                '"' => {
                    self.next();
                    break;
                }
                '\\' => {
                    self.next();
                    if self.eof() {
                        return Err(Error("Unterminated string literal".to_string()));
                    }
                    content.push(match self.peek() {
                        'n' => b'\n',
                        't' => b'\t',
                        c => c as u8,
                    });
                }
                c => {
                    content.push(c as u8);
                }
            }
            self.next();
        }
        Ok(String::from_utf8(content).unwrap())
    }
}

fn ident_to_token(ident: &[u8]) -> Token {
    match ident {
        b"nil" => Token::Nil,
        b"self" => Token::Self_,
        b"if" => Token::If,
        b"then" => Token::Then,
        b"else" => Token::Else,
        b"do" => Token::Do,
        b"end" => Token::End,
        b"while" => Token::While,
        _ => Token::Identifier(String::from_utf8(ident.to_vec()).unwrap()),
    }
}

fn digit_value(c: char) -> usize {
    c as usize - '0' as usize
}

fn is_ident_start(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start(c) || is_digit(c) || c == '?' || c == '!'
}

fn is_symbol_char(c: char) -> bool {
    is_ident_char(c) || c == '='
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_integer_literal_char(c: char) -> bool {
    is_digit(c) || c == '_'
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    fn test_lex(input: &str, expected_result: Result<Vec<Token>>) {
        let result = lex(input).map(|v| v.into_iter().map(|tok| tok.token).collect());
        if result != expected_result {
            panic!("Test failed.\n            input: {:?}\n  expected result: {:?}\n    actual result: {:?}\n",
                   input, expected_result, result);
        }
    }

    #[test]
    fn test_empty() {
        test_lex("", Ok(vec![]));
        test_lex("  \n  ", Ok(vec![]));
    }

    #[test]
    fn test_identifier() {
        test_lex(
            "asdzASDZ_09",
            Ok(vec![Token::Identifier("asdzASDZ_09".to_string())]),
        );
        test_lex(
            "exists?",
            Ok(vec![Token::Identifier("exists?".to_string())]),
        );
        test_lex("send!", Ok(vec![Token::Identifier("send!".to_string())]));
    }

    #[test]
    fn test_symbol() {
        test_lex(
            ":asdzASDZ_09=",
            Ok(vec![Token::Symbol("asdzASDZ_09=".to_string())]),
        );
        test_lex(":then", Ok(vec![Token::Symbol("then".to_string())]));
        test_lex(r#"  :"foo"  "#, Ok(vec![Token::Symbol("foo".to_string())]));
    }

    #[test]
    fn test_integer_literal() {
        test_lex("123", Ok(vec![Token::IntegerLiteral(123)]));
        test_lex("35_000_000", Ok(vec![Token::IntegerLiteral(35_000_000)]));
    }

    #[test]
    fn test_string_literal() {
        test_lex(r#" "" "#, Ok(vec![Token::StringLiteral("".to_string())]));
        test_lex(
            r#" "Hello" "#,
            Ok(vec![Token::StringLiteral("Hello".to_string())]),
        );
        test_lex(
            r#" "a\n\"\\" "#,
            Ok(vec![Token::StringLiteral("a\n\"\\".to_string())]),
        );
    }

    #[test]
    fn test_string_literal_errors() {
        test_lex(
            r#"""#,
            Err(Error("Unterminated string literal".to_string())),
        );
        test_lex(
            r#""\"#,
            Err(Error("Unterminated string literal".to_string())),
        );
    }

    #[test]
    fn test_keywords() {
        test_lex("then", Ok(vec![Token::Then]));
        test_lex("do", Ok(vec![Token::Do]));
        test_lex("self", Ok(vec![Token::Self_]));
    }

    #[test]
    fn test_operators() {
        test_lex(
            "( ) = | @ , ; .",
            Ok(vec![
                Token::LParen,
                Token::RParen,
                Token::Equal,
                Token::Pipe,
                Token::At,
                Token::Comma,
                Token::Semicolon,
                Token::Dot,
            ]),
        );
    }

    #[test]
    fn test_comment() {
        test_lex(
            "
            if # This is a comment
            1
            ",
            Ok(vec![Token::If, Token::IntegerLiteral(1)]),
        );
        test_lex("if#comment", Ok(vec![Token::If]));
    }
}
