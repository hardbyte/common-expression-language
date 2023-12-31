use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum RelationOp {
    // "<" | "<=" | ">=" | ">" | "==" | "!=" | "in"
    NotEquals,
    Equals,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    In,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
}

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

#[derive(Clone, Debug, PartialEq)]
pub enum Member {
    // a.b, a[b, c...], a(b, c, ...)
    Attribute(Rc<String>),
    FunctionCall(Vec<Expression>),
    Index(Box<Expression>),
    Fields(Vec<(Rc<String>, Expression)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    Relation(Box<Expression>, RelationOp, Box<Expression>),

    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),

    Atom(Atom),

    Ident(Rc<String>),

    Member(Box<Expression>, Box<Member>),

    Unary(UnaryOp, Box<Expression>),

    List(Vec<Expression>),

    // Associative arrays with int, uint, bool, or string keys
    Map(Vec<(Expression, Expression)>),
}
