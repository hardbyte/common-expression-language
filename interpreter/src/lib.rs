use crate::utils::usize_from_cel_number;
use cel_parser::ast::{ArithmeticOp, Expression, Member, RelationOp, UnaryOp};
use serde_json;
use serde_json::{Number, Value};
use std::collections::HashMap;
use std::rc::Rc;
use types::{CelFunction, CelMap, CelMapKey, CelType, NumericCelType};

// TODO should this really be public?
mod strings;
pub mod types;
mod utils;

pub fn eval(expr: &Expression, vars: &mut Vec<(String, CelType)>) -> Result<CelType, String> {
    match expr {
        Expression::Atom(atom) => Ok(atom.into()),
        Expression::Ident(name) => {
            for (var_name, var_value) in vars.iter() {
                if var_name == name {
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

        Expression::Unary(op, atom) => {
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
        Expression::Relation(lhs, op, rhs) => {
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
                    RelationOp::Equals => Ok(CelType::Bool(a == b)),
                    RelationOp::NotEquals => Ok(CelType::Bool(a != b)),
                    _ => Err(format!(
                        "Binary operation {:?} not supported for String",
                        op
                    )),
                },
                (CelType::NumericCelType(a), CelType::NumericCelType(b)) => match op {
                    // Here we know that both the lhs and rhs are of type CelType::NumericCelType
                    RelationOp::Equals => Ok(CelType::Bool(a == b)),
                    RelationOp::NotEquals => Ok(CelType::Bool(a != b)),
                    RelationOp::LessThan => Ok(CelType::Bool(a < b)),
                    RelationOp::LessThanOrEqual => Ok(CelType::Bool(a <= b)),
                    RelationOp::GreaterThan => Ok(CelType::Bool(a > b)),
                    RelationOp::GreaterThanOrEqual => Ok(CelType::Bool(a >= b)),
                    RelationOp::In => Err(format!(
                        "Unsupported operation 'in' between numerical types"
                    )),
                },
                (CelType::List(a), CelType::List(b)) => match op {
                    RelationOp::Equals => Ok(CelType::Bool(a == b)),
                    RelationOp::NotEquals => Ok(CelType::Bool(a != b)),
                    _ => Err(format!("Unsupported list operation {:?}", op)),
                },
                (a, CelType::List(b)) => match op {
                    RelationOp::In => {
                        // Is a in list b
                        Ok(CelType::Bool(b.contains(&a)))
                    }
                    _ => Err(format!(
                        "Unsupported operation {:?} between {:?} and {:?}",
                        op, a, b
                    )),
                },
                (a, CelType::Map(b)) => match op {
                    RelationOp::In => {
                        // Is a in list b. Note the `into()` converts a to a CelMapKey
                        Ok(CelType::Bool(b.map.contains_key(&a.into())))
                    }
                    _ => Err(format!(
                        "Unsupported operation {:?} between {:?} and {:?}",
                        op, a, b
                    )),
                },

                (_, _) => Err(format!(
                    "Unsupported relation between {:?} and {:?}",
                    lhs, rhs
                )),
            }
        }

        Expression::Arithmetic(lhs, op, rhs) => {
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
                    ArithmeticOp::Add => Ok(CelType::String(Rc::new(format!("{}{}", a, b)))),
                    _ => Err(format!("Unsupported list operation {:?}", op)),
                },
                (CelType::NumericCelType(a), CelType::NumericCelType(b)) => match op {
                    // Here we know that both the lhs and rhs are of type CelType::NumericCelType
                    ArithmeticOp::Multiply => Ok(CelType::NumericCelType(a * b)),
                    ArithmeticOp::Divide => Ok(CelType::NumericCelType(a / b)),
                    ArithmeticOp::Add => Ok(CelType::NumericCelType(a + b)),
                    ArithmeticOp::Subtract => Ok(CelType::NumericCelType(a - b)),
                    ArithmeticOp::Modulus => Ok(CelType::NumericCelType(a % b)),
                },
                (CelType::List(a), CelType::List(b)) => match op {
                    ArithmeticOp::Add => {
                        let mut output: Vec<CelType> = Vec::with_capacity(a.len() + b.len());
                        output.extend_from_slice(&a);
                        output.extend_from_slice(&b);
                        Ok(CelType::List(Rc::new(output)))
                    }
                    _ => Err(format!("Unsupported list operation {:?}", op)),
                },
                (_, _) => Err(format!(
                    "Unsupported binary op between {:?} and {:?}",
                    lhs, rhs
                )),
            }
        }

        Expression::List(exprs) => {
            let mut output: Vec<CelType> = Vec::with_capacity(exprs.len());
            // Evaluate each expression in the list
            for expr in exprs {
                output.push(eval(expr, vars)?);
            }

            Ok(CelType::List(Rc::new(output)))
        }

        Expression::Map(entries) => {
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
        Expression::Member(lhs, Member::Index(index_expression)) => {
            //println!("Evaluating a member[index]");
            // What can we assert about the LHS?
            let evaluated_lhs = eval(lhs, vars)?;
            match evaluated_lhs {
                CelType::String(s) => {
                    let str = s.as_str();
                    // If the LHS is a string, then the index element must evaluate to integers
                    let index: Result<CelType, _> = eval(index_expression, vars);
                    match index {
                        // Match a single numerical index
                        Ok(CelType::NumericCelType(cel_index)) => {
                            let index = strings::str_index_from_cel_number(&cel_index, s.len())?;

                            Ok(CelType::String(Rc::new(str[index..index + 1].to_string())))
                        }
                        Ok(invalid_type) => {
                            Err(format!("Index must be an integer not {:?}", invalid_type))
                        }
                        Err(e) => Err(format!(
                            "Runtime error in evaluating index expression.\n{:?}",
                            e
                        )),
                    }
                }
                CelType::Map(m) => {
                    let evaluated_index_expression = eval(index_expression, vars)?;
                    // Get a MapKey variant from the index expression
                    let map_key = CelMapKey::from(evaluated_index_expression);
                    Ok(m.map.get(&map_key).unwrap().clone())
                }
                CelType::List(l) => {
                    let index = eval(index_expression, vars);
                    match index {
                        // Match a numerical index, otherwise produce an error
                        Ok(CelType::NumericCelType(cel_index)) => {
                            let usize_res = usize_from_cel_number(&cel_index);
                            let e = Err(format!("Index out of bounds"));
                            match usize_res {
                                // // A real implementation wouldn't just create a copy here!
                                Ok(i) if (i < l.len()) => Ok(l.get(i).unwrap().clone()),
                                Ok(_) => e,
                                Err(index_err) => Err(index_err),
                            }
                            // let value = l.get(index).unwrap();

                            // Ok(value.clone())
                        }
                        Ok(invalid_type) => {
                            Err(format!("Index must be an integer not {:?}", invalid_type))
                        }
                        Err(e) => Err(format!(
                            "Runtime error in evaluating index expression.\n{:?}",
                            e
                        )),
                    }
                }
                _ => Err(format!(
                    "Unhandled member operation for {:?}",
                    evaluated_lhs
                )),
            }
        }
        Expression::Member(lhs, Member::FunctionCall(args)) => {
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
        Expression::And(lhs, rhs) => {
            let evaluated_lhs = eval(lhs, vars)?;
            match evaluated_lhs {
                CelType::Bool(b) => {
                    if b {
                        let evaluated_rhs = eval(rhs, vars)?;
                        match evaluated_rhs {
                            CelType::Bool(b) => Ok(CelType::Bool(b)),
                            _ => Err(format!("Can't AND a bool with {:?}", evaluated_rhs)),
                        }
                    } else {
                        Ok(CelType::Bool(false))
                    }
                }
                _ => Err(format!("Can't AND a {:?}", evaluated_lhs)),
            }
        }
        Expression::Or(lhs, rhs) => {
            let evaluated_lhs = eval(lhs, vars)?;
            match evaluated_lhs {
                CelType::Bool(b) => {
                    if b {
                        Ok(CelType::Bool(true))
                    } else {
                        let evaluated_rhs = eval(rhs, vars)?;
                        match evaluated_rhs {
                            CelType::Bool(b) => Ok(CelType::Bool(b)),
                            _ => Err(format!("Can't OR a bool with {:?}", evaluated_rhs)),
                        }
                    }
                }
                _ => Err(format!("Can't OR a {:?}", evaluated_lhs)),
            }
        }
        Expression::Ternary(condition, true_expression, false_expression) => {
            let evaluated_condition = eval(condition, vars)?;
            match evaluated_condition {
                CelType::Bool(b) => {
                    if b {
                        eval(true_expression, vars)
                    } else {
                        eval(false_expression, vars)
                    }
                }
                _ => Err(format!(
                    "Ternary condition must be a bool, got {:?}",
                    evaluated_condition
                )),
            }
        }

        _ => Err(format!("Need to handle expression {:?}", expr)),
    }
}

/** Load JSON from a string and convert it into CelTypes.

Internally this uses serde_json to parse the string into Serde's internal
representation of JSON. We then recursively convert the values into CelTypes.
*/
pub fn map_json_text_to_cel_types(raw_data: &str) -> Result<CelType, String> {
    // First load the file into serde_json's internal representation Value
    let data: Value = serde_json::from_str(&raw_data).map_err(|e| format!("{}", e))?;

    println!("Parsed context data: {:?}", data);

    // Now convert the Value enum into the CelType enum
    let cel_data = json_to_cel(data);

    Ok(cel_data)
}

/// Map JSON data from Serde JSON to CelType
pub fn json_to_cel(data: Value) -> CelType {
    match data {
        Value::Null => CelType::Null,
        Value::Bool(v) => CelType::Bool(v),
        Value::Number(v) => CelType::NumericCelType(serde_json_number_to_numeric_cel_type(v)),
        Value::String(s) => CelType::String(Rc::new(s)),
        Value::Array(v) => {
            CelType::List(Rc::new(v.iter().map(|v| json_to_cel(v.clone())).collect()))
        }
        Value::Object(v) => CelType::Map(CelMap {
            map: Rc::new(
                v.iter()
                    .map(|(k, v)| {
                        (
                            // Note keys from JSON are always strings
                            CelMapKey::String(Rc::new(k.clone())),
                            json_to_cel(v.clone()),
                        )
                    })
                    .collect(),
            ),
        }),
    }
}

fn serde_json_number_to_numeric_cel_type(n: Number) -> NumericCelType {
    if n.is_i64() {
        NumericCelType::Int(n.as_i64().unwrap())
    } else if n.is_u64() {
        NumericCelType::UInt(n.as_u64().unwrap())
    } else if n.is_f64() {
        NumericCelType::Float(n.as_f64().unwrap())
    } else {
        panic!("Unexpected number type")
    }
}

#[cfg(test)]
mod tests {
    use cel_parser::parse_cel_expression;

    use super::*;

    #[test]
    fn test_evaluate_boolean_expression() {
        let result = evaluate_cel_expression("!true");

        assert_eq!(
            result,
            CelType::Bool(false),
            "Expected false, got {:?}",
            result
        );

        let res_str = format!("{:?}", result);
        assert_eq!(res_str, "Bool(false)")
    }

    #[test]
    fn test_evaluate_list_expression() {
        let result = evaluate_cel_expression("size([0,1,2,3,'hello'][4])");

        assert_eq!(result, CelType::NumericCelType(NumericCelType::UInt(5)),);
    }

    #[test]
    fn test_evaluate_map_expression() {
        let result = evaluate_cel_expression("{'a': 1, 'b': 2, 'c': 3}['b']");

        assert_eq!(result, CelType::NumericCelType(NumericCelType::Int(2)),);
    }

    #[test]
    fn test_evaluate_binary_expressions_ints() {
        assert_eq!(evaluate_cel_expression("5 > 3"), CelType::Bool(true));
        assert_eq!(evaluate_cel_expression("5 >= 3"), CelType::Bool(true));
        assert_eq!(evaluate_cel_expression("5 < 3"), CelType::Bool(false));
        assert_eq!(evaluate_cel_expression("5 <= 3"), CelType::Bool(false));
        assert_eq!(evaluate_cel_expression("5 == 3"), CelType::Bool(false));
        assert_eq!(evaluate_cel_expression("5 == 5"), CelType::Bool(true));
    }

    #[test]
    fn test_evaluate_basic_math() {
        assert_eq!(evaluate_cel_expression("5 + 3 == 8"), CelType::Bool(true));
        assert_eq!(
            evaluate_cel_expression("1.0 / 2.0 == 0.5"),
            CelType::Bool(true)
        );
        assert_eq!(
            evaluate_cel_expression("10.0 % 2.0 == 0.0"),
            CelType::Bool(true)
        );
        assert_eq!(
            evaluate_cel_expression("10.0 % 3.0 == 1.0"),
            CelType::Bool(true)
        );
    }

    #[test]
    fn test_negatives_ints() {
        assert_eq!(
            evaluate_cel_expression("-1"),
            CelType::NumericCelType(NumericCelType::Int(-1))
        );
        assert_eq!(
            evaluate_cel_expression("--1"),
            CelType::NumericCelType(NumericCelType::Int(1))
        );
        assert_eq!(
            evaluate_cel_expression("---1"),
            CelType::NumericCelType(NumericCelType::Int(-1))
        );
        assert_eq!(
            evaluate_cel_expression("-(--1)"),
            CelType::NumericCelType(NumericCelType::Int(-1))
        );
        assert_eq!(
            evaluate_cel_expression("---(1)"),
            CelType::NumericCelType(NumericCelType::Int(-1))
        );
    }

    #[test]
    fn test_negatives_uints_convert() {
        assert_eq!(
            evaluate_cel_expression("-1u"),
            CelType::NumericCelType(NumericCelType::Int(-1))
        );
        assert_eq!(
            evaluate_cel_expression("--1u"),
            CelType::NumericCelType(NumericCelType::Int(1))
        );
    }

    #[test]
    fn test_evaluate_list_contains_int() {
        assert_eq!(evaluate_cel_expression("1 in []"), CelType::Bool(false));
        assert_eq!(evaluate_cel_expression("1 in [1]"), CelType::Bool(true));
        assert_eq!(evaluate_cel_expression("1 in [2]"), CelType::Bool(false));
    }

    #[test]
    fn test_evaluate_size_string() {
        assert_eq!(
            evaluate_cel_expression("size('hello') == 5u"),
            CelType::Bool(true)
        );
    }

    #[test]
    fn test_evaluate_and() {
        assert_eq!(evaluate_cel_expression("true && true"), CelType::Bool(true));

        assert_eq!(
            evaluate_cel_expression("true && false"),
            CelType::Bool(false)
        );
        assert_eq!(
            evaluate_cel_expression("false && true"),
            CelType::Bool(false)
        );
    }
    #[test]
    fn test_evaluate_or() {
        assert_eq!(evaluate_cel_expression("true || true"), CelType::Bool(true));

        assert_eq!(
            evaluate_cel_expression("true || false"),
            CelType::Bool(true)
        );
        assert_eq!(
            evaluate_cel_expression("false || true"),
            CelType::Bool(true)
        );
        assert_eq!(
            evaluate_cel_expression("false || false"),
            CelType::Bool(false)
        );
    }

    #[test]
    fn test_evaluate_ternary() {
        assert_eq!(
            evaluate_cel_expression("true ? 'result_true' : 'result_false'"),
            CelType::String("result_true".to_string().into())
        );

        assert_eq!(
            evaluate_cel_expression("false ? 'result_true' : 'result_false'"),
            CelType::String("result_false".to_string().into())
        );

        assert_eq!(
            evaluate_cel_expression("1 == 1 ? 'result_true' : 'result_false'"),
            CelType::String("result_true".to_string().into())
        );
    }

    #[test]
    fn test_evaluate_ternary_numeric() {
        assert_eq!(
            evaluate_cel_expression("true ? 100 : 200"),
            CelType::NumericCelType(NumericCelType::Int(100))
        );
    }

    fn evaluate_cel_expression(s: &str) -> CelType {
        let program_src = String::from(s);
        let default_vars = &mut Vec::new();
        let program = parse_cel_expression(program_src).unwrap();
        let result = eval(&program, default_vars).unwrap();
        result
    }
}
