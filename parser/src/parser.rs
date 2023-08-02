use crate::ast::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};

use chumsky::prelude::*;
use chumsky::Parser;

fn boolean() -> impl Parser<char, Expression, Error = Simple<char>> {
    just("true")
        .to(true)
        .or(just("false").to(false))
        .map(|b| Expression::Atom(Atom::Bool(b)))
}

/// Parses floating point and integer numbers and returns them as [`Expr::Atom(Atom::Double(...))`]
/// or [`Expr::Atom(Atom::Int(...))`] types. The following formats are supported:
/// - `1`
/// - `1.`
/// - `1.0`
/// - `-1`
/// - `-1.0`
/// - `1e10`
/// - `1e-10`
/// - `1E10`
/// - `1E-10`
/// - `-1e10`
/// - `1u`
fn numbers() -> impl Parser<char, Expression, Error = Simple<char>> {
    let digits = text::digits::<char, Simple<char>>(10);

    let frac = just('.').chain::<char, _, _>(digits.clone().or_not());

    let exp = just('e')
        .or(just('E'))
        .chain::<char, _, _>(one_of("+-").or_not())
        .chain::<char, _, _>(digits.clone());

    let float_or_int = text::int::<char, Simple<char>>(10)
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .try_map(|chars, span| {
            let str = chars.into_iter().collect::<String>();

            if let Ok(i) = str.parse::<i64>() {
                Ok(Expression::Atom(Atom::Int(i)))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Expression::Atom(Atom::Float(f)))
            } else {
                Err(Simple::expected_input_found(span, None, None))
            }
        });

    let unsigned_integer = text::int::<char, Simple<char>>(10)
        .then_ignore(just('u'))
        .map(|s: String| Expression::Atom(Atom::UInt(s.as_str().parse().unwrap())));

    choice((unsigned_integer, float_or_int))
        .padded()
        .labelled("number")
}

fn str_inner(
    delimiter: &str,
    escaping: bool,
) -> impl Parser<char, String, Error = Simple<char>> + '_ {
    let unicode = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(4)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let hex_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(2)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let octal_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(3)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 8).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let escape = just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('u').ignore_then(unicode),
        just('x').or(just('X')).ignore_then(hex_code_point),
        octal_code_point,
    )));

    let mut forbidden = just(delimiter).boxed();
    let mut inner_string = forbidden.not().boxed();

    if escaping {
        forbidden = just(delimiter).or(just("\\")).boxed();
        inner_string = forbidden.not().or(escape).boxed();
    }

    inner_string
        .repeated()
        .delimited_by(just(delimiter), just(delimiter))
        .collect::<String>()
}

// Ref https://github.com/01mf02/jaq/blob/main/jaq-parse/src/token.rs
// See also https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295-L354
// A parser for strings; adapted from Chumsky's JSON example parser.
fn str_() -> impl Parser<char, Expression, Error = Simple<char>> {
    let single_quoted_string = str_inner("'", true).labelled("single quoted string");

    let double_quoted_string = str_inner("\"", true).labelled("double quoted string");

    // Raw strings don't interpret escape sequences.

    let single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'", false))
        .labelled("single quoted raw string");

    let double_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("\"", false))
        .labelled("double quoted raw string");

    let triple_single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'''", false))
        .labelled("triple ' quoted string");

    let triple_single_quoted_escaped_string =
        str_inner("'''", true).labelled("triple ' quoted escaped string");

    let triple_double_quoted_string = str_inner("\"\"\"", true).labelled("triple \" quoted string");

    choice((
        triple_single_quoted_raw_string,
        triple_single_quoted_escaped_string,
        triple_double_quoted_string,
        single_quoted_raw_string,
        single_quoted_string,
        double_quoted_raw_string,
        double_quoted_string,
    ))
    .map(|s| Expression::Atom(Atom::String(s.into())))
}

pub fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let ident = text::ident::<char, Simple<char>>()
        .padded()
        .map(Expression::Ident)
        .labelled("identifier");

    let null = just("null")
        .padded()
        .map(|_| Expression::Atom(Atom::Null))
        .labelled("null");

    let literal = choice((numbers(), boolean(), str_(), null)).labelled("literal");

    let attribute_access = just('.').ignore_then(ident.clone()).map(|rhs| match rhs {
        Expression::Ident(name) => Member::Attribute(name),
        _ => panic!("Expected ident!"),
    });

    let expr = recursive(|expr| {
        let expr_in_paren = expr.clone().delimited_by(just('('), just(')'));

        let expr_list = expr
            .clone()
            .padded()
            .separated_by(just(','))
            .then_ignore(just(',').or_not())
            .collect::<Vec<_>>();

        let function_call = just('(')
            .ignore_then(expr_list.clone())
            .then_ignore(just(')'))
            .map(|args: Vec<Expression>| Member::FunctionCall(args))
            .labelled("function call");

        let index_access = just('[')
            .ignore_then(expr.clone())
            .then_ignore(just(']'))
            .map(|arg: Expression| Member::Index(Box::new(arg)))
            .labelled("index");

        let list = expr_list
            .clone()
            // Ignore trailing comma
            .delimited_by(just('['), just(']'))
            .map(|items: Vec<Expression>| Expression::List(items))
            .labelled("list");

        let map_item = expr
            .clone()
            .then_ignore(just(':'))
            .then(expr.clone())
            .padded()
            .labelled("map item");

        let map = map_item
            .clone()
            .separated_by(just(','))
            .delimited_by(just('{'), just('}'))
            .padded()
            .map(|items| Expression::Map(items))
            .labelled("map");

        let primary = choice((
            literal,
            ident,
            expr_in_paren,
            list,
            map,
            // TODO field inits here
        ))
        .labelled("primary")
        .boxed();

        let member = recursive(|member| {
            let member_chain = primary
                .clone()
                .then(
                    choice((
                        attribute_access.clone(),
                        function_call.clone(),
                        index_access.clone(),
                    ))
                    .repeated(),
                )
                .map(|(lhs_expression, members)| {
                    members.into_iter().fold(lhs_expression, |acc, member| {
                        Expression::Member(Box::new(acc), member)
                    })
                })
                .labelled("member");

            choice((member_chain, primary.clone()))
        })
        .boxed();

        let op = |c| just::<char, _, Simple<char>>(c).padded();

        let not = op('!')
            // could be repeated then fold here
            .ignore_then(member.clone())
            .map(|rhs| Expression::Unary(UnaryOp::Not, Box::new(rhs)));

        let negation = op('-')
            .repeated()
            .at_least(1)
            .then(member.clone())
            .foldr(|_op, rhs: Expression| Expression::Unary(UnaryOp::Neg, Box::new(rhs)))
            .labelled("negation");

        let unary = choice((not, negation, member.clone()))
            .padded()
            .boxed()
            .labelled("unary");

        let product_div_op = op('*')
            .to(ArithmeticOp::Multiply)
            .or(op('/').to(ArithmeticOp::Divide));

        let multiplication = unary
            .clone()
            .then(product_div_op.then(unary.clone()).repeated())
            .foldl(|lhs, (binary_op, rhs)| {
                Expression::Arithmetic(Box::new(lhs), binary_op, Box::new(rhs))
            })
            .labelled("product_or_division")
            .boxed();

        let sum_sub_op = op('+')
            .to(ArithmeticOp::Add)
            .or(op('-').to(ArithmeticOp::Subtract));

        let addition = multiplication
            .clone()
            .then(sum_sub_op.then(multiplication.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Arithmetic(Box::new(lhs), op, Box::new(rhs)))
            .labelled("sub_or_sub")
            .boxed();

        let relationship_op = just("==")
            .to(RelationOp::Equals)
            .or(just("!=").to(RelationOp::NotEquals))
            .or(just(">=").to(RelationOp::GreaterThanOrEqual))
            .or(just("<=").to(RelationOp::LessThanOrEqual))
            .or(just('>').to(RelationOp::GreaterThan))
            .or(just('<').to(RelationOp::LessThan))
            .or(just("in").to(RelationOp::In));

        let relation = addition
            .clone()
            .then(relationship_op.then(addition.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Relation(Box::new(lhs), op, Box::new(rhs)))
            .labelled("comparison")
            .boxed();

        relation
    });

    expr.clone()
        .padded()
        .then_ignore(end())
        .labelled("expression")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_bool() {
        assert_eq!(
            boolean().parse("true"),
            Ok(Expression::Atom(Atom::Bool(true)))
        );
        assert_eq!(
            parser().parse("true"),
            Ok(Expression::Atom(Atom::Bool(true)))
        );
        assert_eq!(
            parser().parse("false"),
            Ok(Expression::Atom(Atom::Bool(false)))
        );
        assert_eq!(
            parser().parse("!false"),
            Ok(Expression::Unary(
                UnaryOp::Not,
                Box::new(Expression::Atom(Atom::Bool(false))),
            ))
        );
        assert_eq!(
            parser().parse("!true"),
            Ok(Expression::Unary(
                UnaryOp::Not,
                Box::new(Expression::Atom(Atom::Bool(true))),
            ))
        );
    }

    #[test]
    fn test_boolean_parser() {
        assert_eq!(
            boolean().parse("true"),
            Ok(Expression::Atom(Atom::Bool(true)))
        );
        assert_eq!(
            boolean().parse("false"),
            Ok(Expression::Atom(Atom::Bool(false)))
        );
        assert!(boolean().parse("1").is_err());
        assert!(boolean().parse("tru").is_err());
        assert!(boolean().parse("False").is_err());
    }

    #[test]
    fn test_boolean_not_parser() {
        assert_eq!(
            parser().parse("!true"),
            Ok(Expression::Unary(
                UnaryOp::Not,
                Box::new(Expression::Atom(Atom::Bool(true)))
            ))
        );
        assert_eq!(
            parser().parse("!false"),
            Ok(Expression::Unary(
                UnaryOp::Not,
                Box::new(Expression::Atom(Atom::Bool(false)))
            ))
        );
        assert!(boolean().parse("-true").is_err());
    }

    #[test]
    fn test_parser_binary_bool_expressions() {
        assert_eq!(
            parser().parse("true == true"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Bool(true))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Atom::Bool(true))),
            ))
        );
    }

    #[test]
    fn test_number_parser_unsigned_numbers() {
        //let unsigned_integer = text::int::<char, Simple<char>>(10).then_ignore(just('u')).map(|s: String| Expr::Atom(Atom::UInt(s.as_str().parse().unwrap())));
        //assert_eq!(unsigned_integer.parse("1u"), Ok(Expr::Atom(Atom::UInt(1))));
        assert_eq!(numbers().parse("1u"), Ok(Expression::Atom(Atom::UInt(1))));
        assert_eq!(numbers().parse("1up"), Ok(Expression::Atom(Atom::UInt(1))));
    }

    #[test]
    fn test_number_parser_int() {
        assert_eq!(numbers().parse("1"), Ok(Expression::Atom(Atom::Int(1))));

        // Debatable if this should be allowed. Ref CEL Spec:
        // https://github.com/google/cel-spec/blob/master/doc/langdef.md#numeric-values
        // "negative integers are produced by the unary negation operator"
        assert_eq!(numbers().parse("100"), Ok(Expression::Atom(Atom::Int(100))));
    }

    #[test]
    fn test_number_parser_double() {
        assert_eq!(
            numbers().parse("1e3"),
            Ok(Expression::Atom(Atom::Float(1000.0)))
        );
        assert_eq!(
            numbers().parse("1e-3"),
            Ok(Expression::Atom(Atom::Float(0.001)))
        );
        assert_eq!(
            numbers().parse("1.4e-3"),
            Ok(Expression::Atom(Atom::Float(0.0014)))
        );
    }

    #[test]
    fn test_parser_str() {
        assert_eq!(
            parser().parse("'hi'"),
            Ok(Expression::Atom(Atom::String(String::from("hi").into())))
        );
        assert_eq!(
            parser().parse("'true'"),
            Ok(Expression::Atom(Atom::String(String::from("true").into())))
        );

        assert_eq!(
            parser().parse("'''true\n'''"),
            Ok(Expression::Atom(Atom::String(
                String::from("true\n").into()
            )))
        );
        assert_eq!(
            parser().parse(r##""""He said "Hi I'm Brian".""""##),
            Ok(Expression::Atom(Atom::String(
                String::from("He said \"Hi I'm Brian\".").into()
            )))
        );
    }

    #[test]
    fn test_parser_raw_strings() {
        assert_eq!(
            parser().parse("r'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );
    }

    #[test]
    fn test_parser_ident() {
        assert_eq!(
            parser().parse("a"),
            Ok(Expression::Ident(String::from("a").into()))
        );

        assert_eq!(
            parser().parse("hello "),
            Ok(Expression::Ident(String::from("hello").into()))
        );
    }

    #[test]
    fn test_parser_ident_invalid() {
        assert!(parser().parse("1a").is_err());
    }

    #[test]
    fn test_parser_ident_function_call_no_args() {
        assert_eq!(
            parser().parse("a()"),
            Ok(Expression::Member(
                Box::new(Expression::Ident(String::from("a").into())),
                Member::FunctionCall(vec![]),
            ))
        );
    }

    #[test]
    fn test_parser_ident_function_call_nonempty_args() {
        assert_eq!(
            parser().parse("a(0,1)"),
            Ok(Expression::Member(
                Box::new(Expression::Ident(String::from("a").into())),
                Member::FunctionCall(vec![
                    Expression::Atom(Atom::Int(0)),
                    Expression::Atom(Atom::Int(1))
                ]),
            ))
        );
    }

    #[test]
    fn test_parser_positive_numbers() {
        assert_eq!(parser().parse("1"), Ok(Expression::Atom(Atom::Int(1))));
        assert_eq!(parser().parse("1u"), Ok(Expression::Atom(Atom::UInt(1))));
        assert_eq!(
            parser().parse("1.0"),
            Ok(Expression::Atom(Atom::Float(1.0)))
        );
    }

    #[test]
    fn test_parser_negative_numbers() {
        assert_eq!(
            parser().parse("-1"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Int(1))),
            ))
        );
        assert_eq!(
            parser().parse("-1u"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::UInt(1))),
            ))
        );
        assert_eq!(
            parser().parse("-1e3"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Float(1000.0))),
            ))
        );
        assert_eq!(
            parser().parse("-1e-3"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Float(0.001))),
            ))
        );
        assert_eq!(
            parser().parse("-1.4e-3"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Float(0.0014))),
            ))
        );

        assert!(boolean().parse("!1").is_err());
    }

    #[test]
    fn test_parser_repeated_negatives_numbers() {
        assert_eq!(
            parser().parse("--1"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(1))),
                ))
            ))
        );
    }

    #[test]
    fn test_parser_delimited_expressions() {
        assert_eq!(
            parser().parse("(-((1)))"),
            Ok(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Int(1))),
            ))
        );
    }

    #[test]
    fn test_parser_integer_relations() {
        assert_eq!(
            parser().parse("2 != 3"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::NotEquals,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );
        assert_eq!(
            parser().parse("2 == 3"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::Equals,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );

        assert_eq!(
            parser().parse("2 < 3"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::LessThan,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );

        assert_eq!(
            parser().parse("2 <= 3"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::LessThanOrEqual,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );
    }

    #[test]
    fn test_parser_binary_product_expressions() {
        assert_eq!(
            parser().parse("2 * 3"),
            Ok(Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Multiply,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );
        assert_eq!(
            parser().parse("2 * -3"),
            Ok(Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Multiply,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ))
        );

        assert_eq!(
            parser().parse("2 / -3"),
            Ok(Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Divide,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ))
        );
    }

    #[test]
    fn test_parser_sum_expressions() {
        assert_eq!(
            parser().parse("2 + 3"),
            Ok(Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Add,
                Box::new(Expression::Atom(Atom::Int(3))),
            ))
        );
        assert_eq!(
            parser().parse("2 - -3"),
            Ok(Expression::Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2))),
                ArithmeticOp::Subtract,
                Box::new(Expression::Unary(
                    UnaryOp::Neg,
                    Box::new(Expression::Atom(Atom::Int(3))),
                )),
            ))
        );
    }

    #[test]
    fn test_str_inner_parser() {
        // Taking the idea from
        // REF: https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295

        let triple_single_quoted_escaped_string =
            str_inner("'''", true).labelled("triple ' quoted escaped string");

        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"''''''"),
            Ok(String::from("").into())
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''hello'''"),
            Ok(String::from("hello").into())
        );
        // Check triple quoted strings interpret escape sequences (note this is a rust raw string, not a CEL raw string)
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\n'''"),
            Ok(String::from("\n").into())
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''x''x'''"),
            Ok(String::from("x''x").into())
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"''' '''"),
            Ok(String::from(" ").into())
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\xFF'''"),
            Ok(String::from("ÿ").into())
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\377'''"),
            Ok(String::from("ÿ").into())
        );
    }

    #[test]
    fn test_str_parser() {
        assert_eq!(
            str_().parse("'Hello!'"),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );
        assert_eq!(
            str_().parse(r"'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );

        assert_eq!(
            str_().parse(r"'''hello'''"),
            Ok(Expression::Atom(Atom::String(String::from("hello").into())))
        );
        // Check triple quoted strings interpret escape sequences (note this is a rust raw string, not a CEL raw string)
        assert_eq!(
            str_().parse(r"'''\n'''"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );
    }

    #[test]
    fn test_raw_str_parser() {
        assert_eq!(
            str_().parse(r"r'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
        assert_eq!(
            str_().parse(r"R'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
        assert_eq!(
            str_().parse("r'1'"),
            Ok(Expression::Atom(Atom::String(String::from("1").into())))
        );
        assert_eq!(
            str_().parse("r\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("R\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse(r"r'''hello'''"),
            Ok(Expression::Atom(Atom::String(String::from("hello").into())))
        );
        assert_eq!(
            str_().parse(r"r'''\n'''"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
    }

    #[test]
    fn test_empty_list_parsing() {
        assert_eq!(parser().parse("[]"), Ok(Expression::List(vec![])));
    }

    #[test]
    fn test_int_list_parsing() {
        assert_eq!(
            parser().parse("[1,2,3]"),
            Ok(Expression::List(vec![
                Expression::Atom(Atom::Int(1)),
                Expression::Atom(Atom::Int(2)),
                Expression::Atom(Atom::Int(3)),
            ]))
        );
    }
    #[test]
    fn test_list_index_parsing() {
        assert_eq!(
            parser().parse("[1,2,3][0]"),
            Ok(Expression::Member(
                Box::new(Expression::List(vec![
                    Expression::Atom(Atom::Int(1)),
                    Expression::Atom(Atom::Int(2)),
                    Expression::Atom(Atom::Int(3)),
                ])),
                Member::Index(Box::new(Expression::Atom(Atom::Int(0)))),
            ))
        );
    }

    #[test]
    fn test_mixed_type_list_parsing() {
        assert_eq!(
            parser().parse("['0', 1,2u,3.0, null]"),
            Ok(Expression::List(vec![
                Expression::Atom(Atom::String(String::from("0").into())),
                Expression::Atom(Atom::Int(1)),
                Expression::Atom(Atom::UInt(2)),
                Expression::Atom(Atom::Float(3.0)),
                Expression::Atom(Atom::Null),
            ]))
        );
    }

    #[test]
    fn test_nested_list_parsing() {
        assert_eq!(
            parser().parse("[[], [], [[1]]]"),
            Ok(Expression::List(vec![
                Expression::List(vec![]),
                Expression::List(vec![]),
                Expression::List(vec![Expression::List(vec![Expression::Atom(Atom::Int(1))])]),
            ]))
        );
    }

    #[test]
    fn test_in_list_relation() {
        assert_eq!(
            parser().parse("2 in [2]"),
            Ok(Expression::Relation(
                Box::new(Expression::Atom(Atom::Int(2))),
                RelationOp::In,
                Box::new(Expression::List(vec![Expression::Atom(Atom::Int(2))])),
            ))
        );
    }

    #[test]
    fn test_empty_map_parsing() {
        assert_eq!(parser().parse("{}"), Ok(Expression::Map(vec![])));
    }

    #[test]
    fn test_nonempty_map_parsing() {
        assert_eq!(
            parser().parse("{'a': 1, 'b': 2}"),
            Ok(Expression::Map(vec![
                (
                    Expression::Atom(Atom::String(String::from("a").into())),
                    Expression::Atom(Atom::Int(1))
                ),
                (
                    Expression::Atom(Atom::String(String::from("b").into())).into(),
                    Expression::Atom(Atom::Int(2)),
                )
            ]))
        );
    }

    #[test]
    fn test_nonempty_map_index_parsing() {
        assert_eq!(
            parser().parse("{'a': 1, 'b': 2}[0]"),
            Ok(Expression::Member(
                Box::new(Expression::Map(vec![
                    (
                        Expression::Atom(Atom::String(String::from("a").into())),
                        Expression::Atom(Atom::Int(1))
                    ),
                    (
                        Expression::Atom(Atom::String(String::from("b").into())).into(),
                        Expression::Atom(Atom::Int(2)),
                    )
                ])),
                Member::Index(Box::new(Expression::Atom(Atom::Int(0)))),
            ))
        );
    }
}
