#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    IntegerLiteral(usize),
    StringLiteral(String),
    Identifier(String),

    // Operators

    LParen,
    RParen,
    Equal,
    Pipe,
    Comma,
    Semicolon,

    // Not technically operators, but parsed the same way
    At,
    Colon,

    // Keywords
    Nil,
    If,
    Then,
    Else,
    Do,
    End,
    While,
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenInfo {
   pub token: Token,
   pub pos: usize,
   pub newline_before: bool,
}
