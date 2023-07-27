use cel_parser::ast::Atom;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NumericCelType {
    Int(i64),
    UInt(u64),
    Float(f64),
}

impl ops::Neg for NumericCelType {
    type Output = NumericCelType;

    fn neg(self) -> NumericCelType {
        match self {
            NumericCelType::UInt(x) => NumericCelType::Int(-(x as i64)), // Perhaps should panic instead of coerce
            NumericCelType::Int(x) => NumericCelType::Int(-x),
            NumericCelType::Float(x) => NumericCelType::Float(-x),
        }
    }
}

impl ops::Add<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn add(self, other: NumericCelType) -> Self {
        match (self, other) {
            (NumericCelType::UInt(x), NumericCelType::UInt(y)) => NumericCelType::UInt(x + y),
            (NumericCelType::Int(x), NumericCelType::Int(y)) => NumericCelType::Int(x + y),
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x + y),
            (x, y) => unimplemented!("Unable to evaluate: {:?} + {:?}", x, y),
        }
    }
}

impl ops::Sub<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn sub(self, other: NumericCelType) -> Self {
        match (self, other) {
            (NumericCelType::UInt(x), NumericCelType::UInt(y)) => NumericCelType::UInt(x - y),
            (NumericCelType::Int(x), NumericCelType::Int(y)) => NumericCelType::Int(x - y),
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x - y),
            (x, y) => unimplemented!("Unable to evaluate: {:?} - {:?}", x, y),
        }
    }
}

impl ops::Mul<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn mul(self, other: NumericCelType) -> Self {
        match (self, other) {
            // TODO can I do this generically - if the types match and the underlying types implement Mul?
            (NumericCelType::UInt(x), NumericCelType::UInt(y)) => NumericCelType::UInt(x * y),
            (NumericCelType::Int(x), NumericCelType::Int(y)) => NumericCelType::Int(x * y),
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x * y),
            (x, y) => unimplemented!("Unable to evaluate: {:?} * {:?}", x, y),
        }
    }
}

impl ops::Div<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn div(self, other: NumericCelType) -> Self {
        match (self, other) {
            (NumericCelType::UInt(x), NumericCelType::UInt(y)) => NumericCelType::UInt(x / y),
            (NumericCelType::Int(x), NumericCelType::Int(y)) => NumericCelType::Int(x / y),
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x / y),

            // Fallback will occur with mixed types
            (x, y) => unimplemented!("Unable to evaluate: {:?} / {:?}", x, y),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd)]
pub enum CelMapKey {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(Rc<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CelMap {
    pub map: Rc<HashMap<CelMapKey, CelType>>,
}

impl PartialOrd for CelMap {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Clone)]
pub struct CelFunction {
    //pub args: Rc<HashMap<String, CelType>>,
    pub function: Rc<dyn Fn(Vec<CelType>) -> CelType>,
}

impl Debug for CelFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CelFunction")
            //.field("args", &self.args)
            .field("function", &"function")
            .finish()
    }
}

impl PartialEq for CelFunction {
    fn eq(&self, _other: &Self) -> bool {
        return false;
    }
}

impl PartialOrd for CelFunction {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum CelType {
    Null,
    Bool(bool),
    NumericCelType(NumericCelType),
    Bytes(Rc<Vec<u8>>),
    String(Rc<String>),
    List(Rc<Vec<CelType>>),
    Map(CelMap),

    Function(CelFunction),
}

impl From<CelType> for CelMapKey {
    #[inline(always)]
    fn from(celtype: CelType) -> Self {
        match celtype {
            CelType::NumericCelType(v) => match v {
                NumericCelType::Int(x) => CelMapKey::Int(x),
                NumericCelType::UInt(x) => CelMapKey::UInt(x),
                NumericCelType::Float(_) => unimplemented!("Floats are not allowed as keys"),
            },
            CelType::String(v) => CelMapKey::String(v),
            CelType::Bool(v) => CelMapKey::Bool(v),
            _ => unimplemented!(),
        }
    }
}

impl From<&Atom> for CelType {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Int(i) => CelType::NumericCelType(NumericCelType::Int(*i)),
            Atom::UInt(ui) => CelType::NumericCelType(NumericCelType::UInt(*ui)),
            Atom::Double(f) => CelType::NumericCelType(NumericCelType::Float(*f)),
            Atom::Bool(b) => CelType::Bool(*b),
            Atom::Bytes(b) => CelType::Bytes(b.clone()),
            Atom::Null => CelType::Null,
            Atom::String(s) => CelType::String(s.clone()),
        }
    }
}
