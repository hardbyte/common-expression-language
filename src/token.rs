
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),

    NumberLiteral(String),

    StringLiteral(String),

    BytesLiteral(Vec<u8>),

    Bool(bool),

    CtrlChar(char),

    Reserved(String),

    Null,
}
