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
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
    MethodCall {
        receiver: Option<Box<Expr>>,
        method: Identifier,
        args: Vec<Expr>,
        block: Option<Box<Block>>,
    },
    Block(Box<Expr>),
    Sequence(Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
}

pub type Param = Identifier;

#[derive(PartialEq, Eq, Debug)]
pub struct Block {
    pub params: Vec<Param>,
    pub body: Expr,
}
