use crate::ast::{Atom, ArithmeticOp, Expression, MemberOp, UnaryOp};

use chumsky::prelude::*;
use chumsky::Parser;

fn boolean() -> impl Parser<char, Expression, Error = Simple<char>> {
    just("true")
        .to(true)
        .or(just("false").to(false))
        .map(|b| Expression::Atom(Atom::Bool(b)))
}

#[test]
fn test_boolean_parser() {
    assert_eq!(boolean().parse("true"), Ok(Expression::Atom(Atom::Bool(true))));
    assert_eq!(boolean().parse("false"), Ok(Expression::Atom(Atom::Bool(false))));
    assert!(boolean().parse("tru").is_err());
    assert!(boolean().parse("False").is_err());
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

    let float_or_int = just('-')
        .or_not()
        .chain::<char, _, _>(text::int::<char, Simple<char>>(10))
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .try_map(|chars, span| {
            let str = chars.into_iter().collect::<String>();

            if let Ok(i) = str.parse::<i64>() {
                Ok(Expression::Atom(Atom::Int(i)))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Expression::Atom(Atom::Double(f)))
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
    assert_eq!(numbers().parse("-100"), Ok(Expression::Atom(Atom::Int(-100))));
}

#[test]
fn test_number_parser_double() {
    assert_eq!(numbers().parse("1e3"), Ok(Expression::Atom(Atom::Double(1000.0))));
    assert_eq!(
        numbers().parse("-1e-3"),
        Ok(Expression::Atom(Atom::Double(-0.001)))
    );
    assert_eq!(
        numbers().parse("-1.4e-3"),
        Ok(Expression::Atom(Atom::Double(-0.0014)))
    );
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
        Ok(Expression::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse("\"Hello!\""),
        Ok(Expression::Atom(Atom::String(String::from("Hello!").into())))
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
        Ok(Expression::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse("R\"Hello!\""),
        Ok(Expression::Atom(Atom::String(String::from("Hello!").into())))
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

pub fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let ident = text::ident::<char, Simple<char>>()
        .padded()
        .map(Expression::Var)
        .labelled("identifier");

    let null = just("null")
        .padded()
        .map(|_| Expression::Atom(Atom::Null))
        .labelled("null");

    let expr = recursive(|expr| {
        let literal = choice((numbers(), boolean(), str_(), null)).labelled("literal");

        let expr_list = expr
            .clone()
            .padded()
            .separated_by(just(','))
            .then_ignore(just(',').or_not())
            .collect::<Vec<_>>();

        let function_call = ident
            .clone()
            .then_ignore(just('('))
            .then(expr_list.clone())
            .then_ignore(just(')'))
            .map(|(name, args)| Expression::Member(Box::new(name), MemberOp::Call(args)))
            .padded()
            .labelled("function call");

        // TODO support "a.b[0]"
        // attribute access should a recursive "member" expression
        let member = ident.clone();

        let attribute_access = ident
            .clone()
            .then(just('.').ignore_then(member.clone()).repeated().at_least(1))
            .foldl(|lhs, rhs| match rhs {
                Expression::Var(v) => Expression::Member(Box::new(lhs), MemberOp::Attribute(v)),
                _ => panic!("Expected identifier after '.'"),
            })
            .labelled("attribute");

        // let index_access = ident
        //     .clone()
        //     .then_ignore(just('['))
        //     .then(expr.clone())
        //     .then_ignore(just(']'))
        //     .map(|(member, rhs)| Expr::Member(Box::new(member), MemberOp::Index(Box::new(rhs))))
        //     .padded()
        //     .labelled("index access");

        let index = expr_list
            .clone()
            .delimited_by(just('['), just(']'))
            .labelled("index");

        let index_access = ident
            .then(index.repeated().at_least(1))
            .foldl(|lhs, rhs| Expression::Member(Box::new(lhs), MemberOp::Index(rhs)))
            .labelled("index access");

        let list = expr_list
            .clone()
            // Ignore trailing comma
            .delimited_by(just('['), just(']'))
            .map(|items| Expression::List(items))
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

        let primary = function_call
            .or(attribute_access)
            .or(index_access)
            .or(literal)
            .or(expr
                .clone()
                .delimited_by(just('('), just(')'))
                .labelled("parenthesized expression"))
            .or(list)
            .or(map)
            .or(ident)
            .padded()
            .labelled("primary")
            .boxed();

        let op = |c| just::<char, _, Simple<char>>(c).padded();

        let not = op('!')
            .ignore_then(primary.clone())
            .map(|rhs| Expression::Unary(UnaryOp::Not, Box::new(rhs)));

        let negation = op('-')
            .ignore_then(primary.clone())
            .map(|rhs| Expression::Unary(UnaryOp::Neg, Box::new(rhs)))
            .labelled("negation");

        let unary = choice((not, negation))
            .or(primary)
            .padded()
            .boxed()
            .labelled("unary");

        let product_div_op = op('*').to(ArithmeticOp::Multiply).or(op('/').to(ArithmeticOp::Divide));

        let multiplication = unary
            .clone()
            .then(product_div_op.then(unary.clone()).repeated())
            .foldl(|lhs, (binary_op, rhs)| Expression::Binary(Box::new(lhs), binary_op, Box::new(rhs)))
            .labelled("product_or_division")
            .boxed();

        let sum_sub_op = op('+').to(ArithmeticOp::Add).or(op('-').to(ArithmeticOp::Subtract));

        let addition = multiplication
            .clone()
            .then(sum_sub_op.then(multiplication.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs)))
            .labelled("sub_or_sub")
            .boxed();

        let relationship_op = just("==")
            .to(ArithmeticOp::Equals)
            .or(just("!=").to(ArithmeticOp::NotEquals))
            .or(just(">=").to(ArithmeticOp::GreaterThanOrEqual))
            .or(just("<=").to(ArithmeticOp::LessThanOrEqual))
            .or(just('>').to(ArithmeticOp::GreaterThan))
            .or(just('<').to(ArithmeticOp::LessThan));

        let relation = addition
            .clone()
            .then(relationship_op.then(addition.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs)))
            .labelled("comparison")
            .boxed();

        relation
    });

    expr.then_ignore(end())
}

#[test]
fn test_parser_bool() {
    assert_eq!(parser().parse("true"), Ok(Expression::Atom(Atom::Bool(true))));
    assert_eq!(parser().parse("false"), Ok(Expression::Atom(Atom::Bool(false))));
    assert_eq!(
        parser().parse("!false"),
        Ok(Expression::Unary(
            UnaryOp::Not,
            Box::new(Expression::Atom(Atom::Bool(false)))
        ))
    );
    assert_eq!(
        parser().parse("!true"),
        Ok(Expression::Unary(
            UnaryOp::Not,
            Box::new(Expression::Atom(Atom::Bool(true)))
        ))
    );
}

#[test]
fn test_parser_binary_bool_expressions() {
    assert_eq!(
        parser().parse("true == true"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Bool(true))),
            ArithmeticOp::Equals,
            Box::new(Expression::Atom(Atom::Bool(true)))
        ))
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
        Ok(Expression::Atom(Atom::String(String::from("true\n").into())))
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
fn test_parser_positive_numbers() {
    assert_eq!(parser().parse("1"), Ok(Expression::Atom(Atom::Int(1))));
    assert_eq!(parser().parse("1u"), Ok(Expression::Atom(Atom::UInt(1))));
    assert_eq!(parser().parse("1.0"), Ok(Expression::Atom(Atom::Double(1.0))));
}

#[test]
fn test_parser_negative_numbers() {
    assert_eq!(
        parser().parse("-1"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::Int(1)))
        ))
    );
    assert_eq!(
        parser().parse("-1u"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::UInt(1)))
        ))
    );
    assert_eq!(
        parser().parse("-1e3"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::Double(1000.0)))
        ))
    );
    assert_eq!(
        parser().parse("-1e-3"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::Double(0.001)))
        ))
    );
    assert_eq!(
        parser().parse("-1.4e-3"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::Double(0.0014)))
        ))
    );
}

#[test]
fn test_parser_delimited_expressions() {
    assert_eq!(
        parser().parse("(-((1)))"),
        Ok(Expression::Unary(
            UnaryOp::Neg,
            Box::new(Expression::Atom(Atom::Int(1)))
        ))
    );
}

#[test]
fn test_parser_binary_product_expressions() {
    assert_eq!(
        parser().parse("2 * 3"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Int(2))),
            ArithmeticOp::Multiply,
            Box::new(Expression::Atom(Atom::Int(3)))
        ))
    );
    assert_eq!(
        parser().parse("2 * -3"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Int(2))),
            ArithmeticOp::Multiply,
            Box::new(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Int(3)))
            ))
        ))
    );

    assert_eq!(
        parser().parse("2 / -3"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Int(2))),
            ArithmeticOp::Divide,
            Box::new(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Int(3)))
            ))
        ))
    );
}

#[test]
fn test_parser_sum_expressions() {
    assert_eq!(
        parser().parse("2 + 3"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Int(2))),
            ArithmeticOp::Add,
            Box::new(Expression::Atom(Atom::Int(3)))
        ))
    );
    assert_eq!(
        parser().parse("2 - -3"),
        Ok(Expression::Binary(
            Box::new(Expression::Atom(Atom::Int(2))),
            ArithmeticOp::Subtract,
            Box::new(Expression::Unary(
                UnaryOp::Neg,
                Box::new(Expression::Atom(Atom::Int(3)))
            ))
        ))
    );
}
