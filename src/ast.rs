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

    // "<" | "<=" | ">=" | ">" | "==" | "!=" | "in"
    // Could be BinaryRel instead, but not sure if that's necessary
    // In,
    NotEquals,
    Equals,
    // GreaterThan,
    // GreaterThanOrEqual,
    // LessThan,
    // LessThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberOp {
    // a.b, a[b], a(b)
    Attribute(String),
    Index(Box<Expr>),
    Call(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Atom(Atom),

    Var(String),

    Member(Box<Expr>, MemberOp),

    Unary(UnaryOp, Box<Expr>),

    Binary(Box<Expr>, BinaryOp, Box<Expr>),

    List(Vec<Expr>),

    // Associative arrays with int, uint, bool, or string keys
    Map(Vec<(Expr, Expr)>),
}
