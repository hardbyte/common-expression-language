use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use chumsky::Parser;

use std::ops;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Int(i64),
    UInt(u64),
    Double(f64),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone)]
enum Expr {
    Atom(Atom),

    Var(String),

    Neg(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),

    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
}

/// Parses floating point and integer numbers and returns them as [`Expression::Atom(Atom::Float(...))`]
/// or [`Expr::Atom(Atom::Int(...))`] types. The following formats are supported:
/// - `1`
/// - `1.0`
/// - `-1`
/// - `-1.0`
/// - `1e10`
/// - `1e-10`
/// - `1E10`
/// - `1E-10`
/// - `-1e10`
fn numbers<'a>() -> impl Parser<char, Expr, Error = Simple<char>> {
    let digits = text::digits::<char, Simple<char>>(10);

    let frac = just('.')
        .chain::<char, _, _>(digits.clone());

    let exp = just('e')
        .or(just('E'))
        .chain::<char, _, _>(
            one_of("+-").or_not()
        )
        .chain::<char, _, _>(digits.clone());


    let floating = just('-')
        .or_not()
        .chain::<char, _, _>(text::int::<char, Simple<char>>(10))
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())

        .try_map(|chars, span| {
            let str = chars.into_iter().collect::<String>();
            println!("PARSER: {:?}", str);
            if let Ok(i) = str.parse::<i64>() {
                Ok(Expr::Atom(Atom::Int(i)))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Expr::Atom(Atom::Double(f)))
            } else {
                Err(Simple::expected_input_found(span, None, None))
            }

        });
        //.boxed();


    let integer = text::int(10).map(|s: String| Expr::Atom(Atom::Int(s.as_str().parse().unwrap())));

    choice((floating, integer)).padded()
    //integer
}

fn parser<'a>() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {

        let number = numbers();

        let atom = number
            .or(expr.delimited_by(just('('), just(')')))
            .or(ident.map(Expr::Var));
        atom

        //        let op = |c| just(c).padded();
        //
        //        let unary = op('-')
        //            .repeated()
        //            .then(atom)
        //            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));
        //
        //        let product = unary.clone()
        //            .then(op('*').to(Expr::Mul as fn(_, _) -> _)
        //                .or(op('/').to(Expr::Div as fn(_, _) -> _))
        //                .then(unary)
        //                .repeated())
        //            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        //
        //        let sum = product.clone()
        //            .then(op('+').to(Expr::Add as fn(_, _) -> _)
        //                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
        //                .then(product)
        //                .repeated())
        //            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        //        sum
    });

    //    let decl = recursive(|decl| {
    //        let r#let = text::keyword("let")
    //            .ignore_then(ident)
    //            .then_ignore(just('='))
    //            .then(expr.clone())
    //            .then_ignore(just(';'))
    //            .then(decl)
    //            .map(|((name, rhs), then)| Expr::Let {
    //                name,
    //                rhs: Box::new(rhs),
    //                then: Box::new(then)
    //            });
    //
    //        r#let.or(expr).padded()
    //    });

    //    decl.then_ignore(end())
    expr.then_ignore(end())
}

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
            NumericCelType::UInt(x) => NumericCelType::Int(-(x as i64)), // Perhas should panic instead of coerce
            NumericCelType::Int(x) => NumericCelType::Int(-x),
            NumericCelType::Float(x) => NumericCelType::Float(-x),
        }
    }
}
impl ops::Add<NumericCelType> for NumericCelType {
    type Output = NumericCelType;

    fn add(self, other: Self) -> Self {
        self + other
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

        //Expr::Neg(a) => Ok(-eval(a, vars)?),

        //        Expr::Add(a, b) => Ok(eval(a, vars)? + eval(b, vars)?),
        //        Expr::Sub(a, b) => Ok(eval(a, vars)? - eval(b, vars)?),
        //        Expr::Mul(a, b) => Ok(eval(a, vars)? * eval(b, vars)?),
        //        Expr::Div(a, b) => Ok(eval(a, vars)? / eval(b, vars)?),
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

    match parser().parse(src) {
        Ok(ast) => {
            println!("AST: \n{:?}\n", ast);
            println!("Evaluating program");
            match eval(&ast, &mut Vec::new()) {
                Ok(output) => println!("{:?}", output),
                Err(eval_err) => println!("Evaluation error: {}", eval_err),
            }
        }
        Err(parse_errs) => {
            println!("An error occured!");
            //            parse_errs
            //            .into_iter()
            //            .for_each(|e| println!("Parse error: {:?}", e)),
        }
    }
}
