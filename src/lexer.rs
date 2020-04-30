use crate::token::{Token, TokenInfo};

pub type Result<A> = ::std::result::Result<A, Error>;

#[derive(PartialEq, Eq, Debug)]
pub struct Error(String);

pub fn lex(input: &str) -> Result<Vec<TokenInfo>> {
    Ok(vec![])
}

struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    fn lex(&mut self) {
    }
}

#[cfg(test)] mod tests {
    use crate::lexer::*;

    fn test_lex(input: &str, expected_result: Result<Vec<Token>>) {
        assert_eq!(expected_result,
                   lex(input).map(|v| v.into_iter().map(|tok| tok.token).collect()));
    }

    #[test]
    fn test_basic_tokens() {
        test_lex("foo", Ok(vec![Token::Identifier("foo".to_string())]));
    }
}
