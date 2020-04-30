pub type Identifier = String;

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    NilLiteral,
    SelfLiteral,
    IntegerLiteral(usize),
    StringLiteral(String),
    Var(Identifier),
    Symbol(Identifier),
    InstanceVariable(Identifier),
    If {
        condition: Box<Expr>,
        if_true: Box<Expr>,
        if_false: Option<Box<Expr>>,
    },
    MethodCall {
        receiver: Option<Box<Expr>>,
        method: Identifier,
        args: Box<[Expr]>,
        block: Option<Block>,
    },
    Block(Statement),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Block {}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {}
