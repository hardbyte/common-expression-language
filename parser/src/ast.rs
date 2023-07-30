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
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberOp {
    // a.b, a[b, c...], a(b, c, ...)
    Attribute(String),
    Index(Vec<Expression>),
    Call(Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Atom(Atom),

    Var(String),

    Member(Box<Expression>, MemberOp),

    Unary(UnaryOp, Box<Expression>),

    Binary(Box<Expression>, BinaryOp, Box<Expression>),

    List(Vec<Expression>),

    // Associative arrays with int, uint, bool, or string keys
    Map(Vec<(Expression, Expression)>),
}
