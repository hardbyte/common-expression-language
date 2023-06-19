use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use chumsky::Parser;

use cel_parser::ast::{Atom, BinaryOp, Expr, UnaryOp};
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
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x + y),

            (x, y) => x + y,
        }
    }
}

impl ops::Sub<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn sub(self, other: NumericCelType) -> Self {
        match (self, other) {
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x - y),
            (x, y) => x - y,
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

            // Fallback match
            (x, y) => x * y,
        }
    }
}

impl ops::Div<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn div(self, other: NumericCelType) -> Self {
        match (self, other) {
            // TODO
            (NumericCelType::UInt(x), NumericCelType::UInt(y)) => NumericCelType::UInt(x / y),
            (NumericCelType::Int(x), NumericCelType::Int(y)) => NumericCelType::Int(x / y),
            (NumericCelType::Float(x), NumericCelType::Float(y)) => NumericCelType::Float(x / y),

            // Fallback match
            (x, y) => x / y,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum CelType {
    Null,
    Bool(bool),

    NumericCelType(NumericCelType),

    Bytes(Rc<Vec<u8>>),
    String(Rc<String>),
    //    List(Rc<[CelType]>),
    //    Map(CelMap),
    //    Function(Rc<String>, Option<Box<CelType>>),
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
        // Expr::Add(a, b) => Ok(eval(a, vars)? + eval(b, vars)?),
        // Expr::Sub(a, b) => Ok(eval(a, vars)? - eval(b, vars)?),
        // Expr::Mul(a, b) => Ok(eval(a, vars)? * eval(b, vars)?),
        // Expr::Div(a, b) => Ok(eval(a, vars)? / eval(b, vars)?),
        //
        //        Expr::Var(name) => if let Some((_, val)) = vars.iter().rev().find(|(var, _)| *var == name) {
        //            Ok(*val)
        //        } else {
        //            Err(format!("Can't find variables `{}` in scope", name))
        //        },
        //        Expr::Let {name, rhs, then } => {
        //            let rhs = eval(rhs, vars)?;
        //            vars.push((name, rhs));
        //            let output = eval(then, vars);
        //            vars.pop();
        //            output
        //        },
        _ => todo!(), // We'll handle other cases later
    }
}

fn main() {
    let src = std::env::args().nth(1).unwrap();

    println!("Loaded source. {:?}", src);

    match parser::parser().parse(src) {
        Ok(ast) => {
            println!("AST: \n{:?}\n", ast);
            println!("Evaluating program");
            match eval(&ast, &mut Vec::new()) {
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
