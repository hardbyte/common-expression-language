use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Int(i64),
    UInt(u64),
    Float(f64),
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
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelationOp {
    // "<" | "<=" | ">=" | ">" | "==" | "!=" | "in"
    NotEquals,
    Equals,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Member {
    // a.b, a[b, c...], a(b, c, ...)
    Attribute(String),
    FunctionCall(Vec<Expression>),
    Index(Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Atom(Atom),

    Ident(String),

    Member(Box<Expression>, Member),

    Unary(UnaryOp, Box<Expression>),

    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    Relation(Box<Expression>, RelationOp, Box<Expression>),

    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),

    List(Vec<Expression>),

    // Associative arrays with int, uint, bool, or string keys
    Map(Vec<(Expression, Expression)>),
}
