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
        args: Vec<Expr>,
        block: Option<Block>,
    },
    Block(Box<Expr>),
    Sequence(Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Block {}
