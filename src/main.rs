use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;
use chumsky::Parser;

use std::ops;
use std::rc::Rc;
use cel_parser::ast::{Atom, Expr, UnaryOp};
use cel_parser::parser;

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
            (x, y) => x + y
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
                UnaryOp::Neg => {
                    match inner {
                        CelType::NumericCelType(nct) => Ok(CelType::NumericCelType(-nct)),
                        _ => Err(format!("Can't negate non-numeric type {:?}", inner)),
                    }
                }
                UnaryOp::Not => todo!("Implement 'Not' unary operator"),
            }


        }

        // Expr::Binary(op, a, b) => {
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
