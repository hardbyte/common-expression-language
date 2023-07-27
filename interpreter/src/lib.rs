use cel_parser::ast::{BinaryOp, Expr, MemberOp, UnaryOp};
use std::collections::HashMap;
use std::rc::Rc;
use types::{CelFunction, CelMap, CelMapKey, CelType, NumericCelType};

// TODO should this really be public?
mod strings;
pub mod types;

pub fn eval<'a>(expr: &'a Expr, vars: &mut Vec<(&'a String, CelType)>) -> Result<CelType, String> {
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
                return Err(format!("Macro '{}' not implemented", name));
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
            // For now I evaluate both sides and then destruct matching types
            // then match the operation. However this is not ideal as it will
            // evaluate both sides even if it is not needed.

            let eval_rhs = eval(rhs, vars)?;

            // Not every CelType implement all binary ops.
            // It might make sense to flip it and match first on the operation,
            // this would mean operations such as `==` and `!=` could have one implementation
            // regardless of the sides' types.
            match (eval_lhs, eval_rhs) {
                (CelType::String(a), CelType::String(b)) => match op {
                    BinaryOp::Add => Ok(CelType::String(Rc::new(format!("{}{}", a, b)))),
                    BinaryOp::Equals => Ok(CelType::Bool(a == b)),
                    BinaryOp::NotEquals => Ok(CelType::Bool(a != b)),
                    _ => Err(format!(
                        "Binary operation {:?} not supported for String",
                        op
                    )),
                },
                (CelType::NumericCelType(a), CelType::NumericCelType(b)) => match op {
                    // Here we know that both the lhs and rhs are of type CelType::NumericCelType
                    BinaryOp::Mul => Ok(CelType::NumericCelType(a * b)),
                    BinaryOp::Div => Ok(CelType::NumericCelType(a / b)),
                    BinaryOp::Add => Ok(CelType::NumericCelType(a + b)),
                    BinaryOp::Sub => Ok(CelType::NumericCelType(a - b)),
                    BinaryOp::Equals => Ok(CelType::Bool(a == b)),
                    BinaryOp::NotEquals => Ok(CelType::Bool(a != b)),
                    BinaryOp::LessThan => Ok(CelType::Bool(a < b)),
                    BinaryOp::LessThanOrEqual => Ok(CelType::Bool(a <= b)),
                    BinaryOp::GreaterThan => Ok(CelType::Bool(a > b)),
                    BinaryOp::GreaterThanOrEqual => Ok(CelType::Bool(a >= b)),
                },
                (CelType::List(a), CelType::List(b)) => match op {
                    BinaryOp::Add => {
                        let mut output: Vec<CelType> = Vec::with_capacity(a.len() + b.len());
                        output.extend_from_slice(&a);
                        output.extend_from_slice(&b);
                        Ok(CelType::List(Rc::new(output)))
                    }
                    BinaryOp::Equals => Ok(CelType::Bool(a == b)),
                    BinaryOp::NotEquals => Ok(CelType::Bool(a != b)),
                    _ => Err(format!("Unsupported list operation {:?}", op)),
                },
                (_, _) => Err(format!(
                    "Unsupported binary op between {:?} and {:?}",
                    lhs, rhs
                )),
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
        Expr::Member(lhs, MemberOp::Index(index_expressions)) => {
            println!("Evaluating a member[index]");
            // What can we assert about the LHS?
            let evaluated_lhs = eval(lhs, vars)?;
            match evaluated_lhs {
                CelType::String(s) => {
                    let str = s.as_str();
                    // If the LHS is a string, then the index elements must evaluate to integers
                    let evaluated_indexes: Result<Vec<CelType>, _> = index_expressions
                        .iter()
                        .map(|index_expr| eval(index_expr, vars))
                        .collect();

                    let indexes = evaluated_indexes?;

                    match indexes.as_slice() {
                        // Match a single index
                        [CelType::NumericCelType(cel_index)] => {
                            let index = strings::str_index_from_cel_number(cel_index, s.len())?;

                            Ok(CelType::String(Rc::new(str[index..index + 1].to_string())))
                        }
                        // Match a pair of indexes
                        [CelType::NumericCelType(start_index), CelType::NumericCelType(end_index)] =>
                        {
                            // String slice "hello"[1,3] -> "el"
                            let str_len = s.len();
                            let start = strings::str_index_from_cel_number(start_index, str_len)?;
                            let end = strings::str_index_from_cel_number(end_index, str_len)?;
                            Ok(CelType::String(Rc::new(str[start..end].to_string())))
                        }
                        _ => Err(format!("Index must be an integer")),
                    }
                }
                CelType::Map(m) => {
                    // First get a MapKey variant from the (first) index expression
                    let first_index_expression = eval(&index_expressions[0], vars)?;

                    let map_key = CelMapKey::from(first_index_expression);
                    Ok(m.map.get(&map_key).unwrap().clone())
                }
                _ => Err(format!(
                    "Unhandled member operation for {:?}",
                    evaluated_lhs
                )),
            }
            //Err(format!("Need to handle member operation"))
        }
        Expr::Member(lhs, MemberOp::Call(args)) => {
            let evaluated_lhs = eval(lhs, vars)?;
            match evaluated_lhs {
                CelType::Function(f) => {
                    // Call the function (evaluate the output?)
                    let mut evaluated_arguments: Vec<CelType> = Vec::with_capacity(args.len());
                    // Evaluate each expression in the list of arguments
                    for expr in args {
                        evaluated_arguments.push(eval(&expr, vars)?);
                    }
                    return Ok((f.function)(evaluated_arguments));
                }
                _ => Err(format!("Can't call this type")),
            }
        }
        _ => Err(format!("Need to handle member operation")),
    }
}