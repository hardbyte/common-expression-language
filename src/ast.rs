use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Int(i64),
    UInt(u64),
    Double(f64),
    // A reference counted String
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Atom),

    Var(String),

    Unary(UnaryOp, Box<Expr>),

    Binary(Box<Expr>, BinaryOp, Box<Expr>),

    Call(String, Vec<Expr>),

    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
}
