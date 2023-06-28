use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use chumsky::Parser;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use cel_parser::ast::{Atom, BinaryOp, Expr, MemberOp, UnaryOp};
use cel_parser::parser;

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
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
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
    fn eq(&self, other: &Self) -> bool {
        return false;
    }
}
impl PartialOrd for CelFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
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

fn eval<'a>(expr: &'a Expr, vars: &mut Vec<(&'a String, CelType)>) -> Result<CelType, String> {
    match expr {
        Expr::Atom(atom) => Ok(atom.into()),
        Expr::Var(name) => {
            for (var_name, var_value) in vars.iter() {
                if *var_name == name {
                    return Ok(var_value.clone());
                }
            }
            // Built in functions should be in a `Context` struct but we will do some
            // simple ones here for now
            if name == "size" {
                // For now just size of List, String
                return Ok(CelType::Function(CelFunction {
                    function: Rc::new(|args| {
                        let s = args.get(0).unwrap();
                        let size: usize = match s {
                            CelType::List(l) => l.len(),
                            CelType::String(s) => s.len(),
                            CelType::Bytes(b) => b.len(),
                            CelType::Map(m) => m.map.len(),
                            _ => unimplemented!(),
                        };

                        CelType::NumericCelType(NumericCelType::UInt(size as u64))
                    }),
                }));
            }
            // Macros are not supported yet
            if name == "has" {
                return Err(format!("Macro {} not implemented", name));
            }
            Err(format!("Variable {} not found", name))
        }

        Expr::Unary(op, atom) => {
            let inner = eval(atom, vars)?;
            match op {
                UnaryOp::Neg => match inner {
                    CelType::NumericCelType(nct) => Ok(CelType::NumericCelType(-nct)),
                    _ => Err(format!("Can't negate non-numeric type {:?}", inner)),
                },
                UnaryOp::Not => match inner {
                    CelType::Bool(b) => Ok(CelType::Bool(!b)),
                    _ => Err(format!("Only boolean expressions can be negated")),
                },
            }
        }
        Expr::Binary(lhs, op, rhs) => {
            let eval_lhs = eval(lhs, vars)?;
            let eval_rhs = eval(rhs, vars)?;

            // Not every CelType implement binary ops. For now we check that
            // both the lhs and rhs are of type CelType::NumericCelType
            match (eval_lhs, eval_rhs) {
                (CelType::NumericCelType(a), CelType::NumericCelType(b)) => match op {
                    BinaryOp::Mul => Ok(CelType::NumericCelType(a * b)),
                    BinaryOp::Div => Ok(CelType::NumericCelType(a / b)),
                    BinaryOp::Add => Ok(CelType::NumericCelType(a + b)),
                    BinaryOp::Sub => Ok(CelType::NumericCelType(a - b)),
                },
                (_, _) => Err(format!("Only numeric types support binary ops currently")),
            }
        }

        Expr::List(exprs) => {
            let mut output: Vec<CelType> = Vec::with_capacity(exprs.len());
            // Evaluate each expression in the list
            for expr in exprs {
                output.push(eval(expr, vars)?);
            }

            Ok(CelType::List(Rc::new(output)))
        }

        Expr::Map(entries) => {
            let mut output: HashMap<CelMapKey, CelType> = HashMap::with_capacity(entries.len());
            // Evaluate each key and expression in the list
            for (key, expr) in entries {
                let evaluated_key = eval(key, vars)?;
                output.insert(evaluated_key.into(), eval(expr, vars)?);
            }

            Ok(CelType::Map(CelMap {
                map: Rc::new(output),
            }))
        }
        Expr::Member(lhs, MemberOp::Call(args)) => {
            println!("Function call");

            let evaluated_lhs = eval(lhs, vars)?;
            println!("LHS evaluated to: {:?}", evaluated_lhs);
            match evaluated_lhs {
                CelType::Function(f) => {
                    // Call the function (evaluate the output?)
                    let mut evaluated_arguments: Vec<CelType> = Vec::with_capacity(args.len());
                    // Evaluate each expression in the list of arguments
                    for expr in args {
                        evaluated_arguments.push(eval(&expr, vars)?);
                    }
                    return Ok((f.function)(evaluated_arguments));

                    // Temporarily just return a true
                    //return Ok(CelType::Bool(true));
                }
                _ => Err(format!("Can't call this type")),
            }
        }
        _ => Err(format!("Need to handle member operation")),
    }
}

fn main() {
    let src = std::env::args().nth(1).unwrap();

    println!("Loaded source. {:?}", src);

    match parser::parser().parse(src) {
        Ok(ast) => {
            println!("AST: \n{:?}\n", ast);
            println!("Evaluating program");
            let default_vars = &mut Vec::new();

            // Create hard coded variables for builtin macros
            let test_var_name = String::from("has");
            default_vars.push((
                &test_var_name,
                CelType::NumericCelType(NumericCelType::Int(1)),
            ));

            match eval(&ast, default_vars) {
                Ok(output) => println!("{:?}", output),
                Err(eval_err) => println!("Evaluation error: {}", eval_err),
            }
        }
        Err(parse_errs) => {
            println!("An error occurred!");
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {:?}", e));
        }
    }
}
