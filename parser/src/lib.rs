pub mod ast;
pub mod parser;

use chumsky::error::Simple;
use chumsky::Parser;

pub fn parse_cel_expression(input: String) -> Result<ast::Expression, Vec<Simple<char>>> {
    // Expose the internal parser::parse function
    parser::parser().parse(input)
}

#[cfg(test)]
mod tests {
    use crate::ast::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
    use crate::parse_cel_expression;

    #[test]
    fn test_parser_errors() {
        assert!(parse_cel_expression("1a".to_string()).is_err());
        assert!(parse_cel_expression("#$^%&*".to_string()).is_err());
        assert!(parse_cel_expression("+".to_string()).is_err());
    }

    fn parse(input: &str) -> Expression {
        crate::parse_cel_expression(input.to_string()).unwrap_or_else(|_e| panic!("Parse error"))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input), expected);
    }

    #[test]
    fn test_parser_bool() {
        assert_parse_eq("true", Expression::Atom(Atom::Bool(true)));
        assert_parse_eq("true", Expression::Atom(Atom::Bool(true)));
        assert_parse_eq("false", Expression::Atom(Atom::Bool(false)));
    }

    #[test]
    fn test_parser_bool_unary_ops() {
        assert_parse_eq(
            "!false",
            Expression::Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(false)))),
        );
        assert_parse_eq(
            "!true",
            Expression::Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(true)))),
        );
    }

    #[test]
    fn test_parser_bool_unary_ops_repeated() {
        assert_eq!(
            parse("!!true"),
            (Expression::Unary(
                UnaryOp::Not,
                Box::new(Expression::Unary(
                    UnaryOp::Not,
                    Box::new(Expression::Atom(Atom::Bool(true))),
                ))
            ))
        );
    }

    #[test]
    fn test_boolean_not_parser() {
        assert_parse_eq(
            "!true",
            Expression::Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(true)))),
        );
        assert_parse_eq(
            "!false",
            Expression::Unary(UnaryOp::Not, Box::new(Expression::Atom(Atom::Bool(false)))),
        );
    }

    #[test]
    fn test_parser_binary_bool_expressions() {
        assert_parse_eq(
            "true == true",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Bool(true))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Atom::Bool(true))),
            ),
        );
    }

    #[test]
    fn test_number_parser_double() {
        assert_parse_eq("1e3", Expression::Atom(Atom::Float(1000.0)));
        assert_parse_eq("1e-3", Expression::Atom(Atom::Float(0.001)));
        assert_parse_eq("1.4e-3", Expression::Atom(Atom::Float(0.0014)));
    }

    #[test]
    fn test_parser_str() {
        assert_parse_eq(
            "'hi'",
            Expression::Atom(Atom::String(String::from("hi").into())),
        );
        assert_parse_eq(
            "'true'",
            Expression::Atom(Atom::String(String::from("true").into())),
        );

        assert_parse_eq(
            "'''true\n'''",
            Expression::Atom(Atom::String(String::from("true\n").into())),
        );
        assert_parse_eq(
            r##""""He said "Hi I'm Brian".""""##,
            Expression::Atom(Atom::String(
                String::from("He said \"Hi I'm Brian\".").into(),
            )),
        );
    }

    #[test]
    fn test_parser_raw_strings() {
        assert_parse_eq(
            "r'\n'",
            Expression::Atom(Atom::String(String::from("\n").into())),
        );
    }

    #[test]
    fn test_parser_ident() {
        assert_parse_eq("a", Expression::Ident(String::from("a").into()));

        assert_parse_eq("hello ", Expression::Ident(String::from("hello").into()));
    }

    #[test]
    fn test_parser_ident_function_call_no_args() {
        assert_parse_eq(
            "a()",
            Expression::Member(
                Box::new(Expression::Ident(String::from("a").into())),
                Member::FunctionCall(vec![]),
            ),
        );
    }

    #[test]
    fn test_parser_ident_function_call_nonempty_args() {
        assert_parse_eq(
            "a(0,1)",
            Expression::Member(
                Box::new(Expression::Ident(String::from("a").into())),
                Member::FunctionCall(vec![
                    Expression::Atom(Atom::Int(0)),
                    Expression::Atom(Atom::Int(1)),
                ]),
            ),
        );
    }

    #[test]
    fn test_parser_positive_numbers() {
        assert_eq!(parse("1"), (Expression::Atom(Atom::Int(1))));
        assert_eq!(parse("1u"), (Expression::Atom(Atom::UInt(1))));
        assert_parse_eq("1.0", Expression::Atom(Atom::Float(1.0)));
    }

    #[test]
    fn test_parser_negative_numbers() {
        assert_parse_eq(
            "-1",
            Expression::Unary(UnaryOp::Neg, Box::new(Expression::Atom(Atom::Int(1)))),
        );
        assert_parse_eq(
            "-1u",
            Expression::Unary(UnaryOp::Neg, Box::new(Expression::Atom(Atom::UInt(1)))),
        );
        assert_parse_eq(
            "-1e3",
            Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Float(1000.0))),
            ),
        );
        assert_parse_eq(
            "-1e-3",
            Expression::Unary(UnaryOp::Neg, Box::new(Expression::Atom(Atom::Float(0.001)))),
        );
        assert_parse_eq(
            "-1.4e-3",
            Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Float(0.0014))),
            ),
        );
    }

    #[test]
    fn test_parser_repeated_negatives_numbers() {
        assert_parse_eq(
            "--1",
            Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(1))),
                )),
            ),
        );
    }

    #[test]
    fn test_parser_delimited_expressions() {
        assert_parse_eq(
            "(-((1)))",
            Expression::Unary(UnaryOp::Neg, Box::new(Expression::Atom(Atom::Int(1)))),
        );
    }

    #[test]
    fn test_parser_integer_relations() {
        assert_parse_eq(
            "2 != 3",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::NotEquals,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );
        assert_parse_eq(
            "2 == 3",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );

        assert_parse_eq(
            "2 < 3",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::LessThan,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );

        assert_parse_eq(
            "2 <= 3",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::LessThanOrEqual,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );
    }

    #[test]
    fn test_parser_binary_product_expressions() {
        assert_parse_eq(
            "2 * 3",
            Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Multiply,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );
        assert_parse_eq(
            "2 * -3",
            Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Multiply,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ),
        );

        assert_parse_eq(
            "2 / -3",
            Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Divide,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ),
        );
    }

    #[test]
    fn test_parser_sum_expressions() {
        assert_parse_eq(
            "2 + 3",
            Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Add,
                Box::new(Expression::Atom(Atom::Int(3))),
            ),
        );
        assert_parse_eq(
            "2 - -3",
            Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Subtract,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ),
        );
    }

    #[test]
    fn test_empty_list_parsing() {
        assert_eq!(parse("[]"), (Expression::List(vec![])));
    }

    #[test]
    fn test_int_list_parsing() {
        assert_parse_eq(
            "[1,2,3]",
            Expression::List(vec![
                Expression::Atom(Atom::Int(1)),
                Expression::Atom(Atom::Int(2)),
                Expression::Atom(Atom::Int(3)),
            ]),
        );
    }

    #[test]
    fn test_list_index_parsing() {
        assert_parse_eq(
            "[1,2,3][0]",
            Expression::Member(
                Box::new(Expression::List(vec![
                    Expression::Atom(Atom::Int(1)),
                    Expression::Atom(Atom::Int(2)),
                    Expression::Atom(Atom::Int(3)),
                ])),
                Member::Index(Box::new(Expression::Atom(Atom::Int(0)))),
            ),
        );
    }

    #[test]
    fn test_mixed_type_list_parsing() {
        assert_parse_eq(
            "['0', 1,2u,3.0, null]",
            Expression::List(vec![
                Expression::Atom(Atom::String(String::from("0").into())),
                Expression::Atom(Atom::Int(1)),
                Expression::Atom(Atom::UInt(2)),
                Expression::Atom(Atom::Float(3.0)),
                Expression::Atom(Atom::Null),
            ]),
        );
    }

    #[test]
    fn test_nested_list_parsing() {
        assert_parse_eq(
            "[[], [], [[1]]]",
            Expression::List(vec![
                Expression::List(vec![]),
                Expression::List(vec![]),
                Expression::List(vec![Expression::List(vec![Expression::Atom(Atom::Int(1))])]),
            ]),
        );
    }

    #[test]
    fn test_in_list_relation() {
        assert_parse_eq(
            "2 in [2]",
            Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::In,
                Box::new(Expression::List(vec![Expression::Atom(Atom::Int(2))])),
            ),
        );
    }

    #[test]
    fn test_empty_map_parsing() {
        assert_eq!(parse("{}"), (Expression::Map(vec![])));
    }

    #[test]
    fn test_nonempty_map_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}",
            Expression::Map(vec![
                (
                    Expression::Atom(Atom::String(String::from("a").into())),
                    Expression::Atom(Atom::Int(1)),
                ),
                (
                    Expression::Atom(Atom::String(String::from("b").into())).into(),
                    Expression::Atom(Atom::Int(2)),
                ),
            ]),
        );
    }

    #[test]
    fn test_conditionals() {
        assert_parse_eq(
            "true && true",
            Expression::And(
                Box::new(Expression::Atom(Atom::Bool(true))),
                Box::new(Expression::Atom(Atom::Bool(true))),
            ),
        );
        assert_parse_eq(
            "false || true",
            Expression::Or(
                Box::new(Expression::Atom(Atom::Bool(false))),
                Box::new(Expression::Atom(Atom::Bool(true))),
            ),
        );
    }

    #[test]
    fn test_ternary_true_condition() {
        assert_parse_eq(
            "true ? 'result_true' : 'result_false'",
            Expression::Ternary(
                Box::new(Expression::Atom(Atom::Bool(true))),
                Box::new(Expression::Atom(Atom::String(
                    "result_true".to_string().into(),
                ))),
                Box::new(Expression::Atom(Atom::String(
                    "result_false".to_string().into(),
                ))),
            ),
        );

        assert_parse_eq(
            "true ? 100 : 200",
            Expression::Ternary(
                Box::new(Expression::Atom(Atom::Bool(true))),
                Box::new(Expression::Atom(Atom::Int(100))),
                Box::new(Expression::Atom(Atom::Int(200))),
            ),
        );
    }

    #[test]
    fn test_ternary_false_condition() {
        assert_parse_eq(
            "false ? 'result_true' : 'result_false'",
            Expression::Ternary(
                Box::new(Expression::Atom(Atom::Bool(false))),
                Box::new(Expression::Atom(Atom::String(
                    "result_true".to_string().into(),
                ))),
                Box::new(Expression::Atom(Atom::String(
                    "result_false".to_string().into(),
                ))),
            ),
        );
    }

    #[test]
    fn test_nonempty_map_index_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}[0]",
            Expression::Member(
                Box::new(Expression::Map(vec![
                    (
                        Expression::Atom(Atom::String(String::from("a").into())),
                        Expression::Atom(Atom::Int(1)),
                    ),
                    (
                        Expression::Atom(Atom::String(String::from("b").into())).into(),
                        Expression::Atom(Atom::Int(2)),
                    ),
                ])),
                Member::Index(Box::new(Expression::Atom(Atom::Int(0)))),
            ),
        );
    }

    #[test]
    fn test_field_init() {
        assert_parse_eq(
            "GeoPoint{ latitude: 10.0, longitude: 5.5 }",
            Expression::Member(
                Box::new(Expression::Ident(String::from("GeoPoint").into())),
                Member::Fields(vec![
                    (
                        String::from("latitude").into(),
                        Expression::Atom(Atom::Float(10.0)),
                    ),
                    (
                        String::from("longitude").into(),
                        Expression::Atom(Atom::Float(5.5)),
                    ),
                ]),
            ),
        );
    }

    #[test]
    fn test_nested_field_init() {
        assert_parse_eq(
            "common.GeoPoint{ latitude: 10.0, longitude: 5.5 }",
            Expression::Member(
                Box::new(Expression::Member(
                    Box::new(Expression::Ident(String::from("common").into())),
                    Member::Attribute(String::from("GeoPoint")),
                )),
                Member::Fields(vec![
                    (
                        String::from("latitude").into(),
                        Expression::Atom(Atom::Float(10.0)),
                    ),
                    (
                        String::from("longitude").into(),
                        Expression::Atom(Atom::Float(5.5)),
                    ),
                ]),
            ),
        );
    }
}
