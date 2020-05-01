#[derive(PartialEq, Eq, Debug, Clone)]
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
    Dot,

    // Not technically operators, but parsed the same way
    At,
    Colon,

    // Keywords
    Nil,
    Self_,
    If,
    Then,
    Else,
    Do,
    End,
    While,

    EOF,
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenInfo {
    pub token: Token,
    pub start: usize,
    pub end: usize,
    pub newline_before: bool,
}
