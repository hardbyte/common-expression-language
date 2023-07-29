use crate::ast::{Atom, BinaryOp, Expr, MemberOp, UnaryOp};

use chumsky::prelude::*;
use chumsky::Parser;

fn boolean<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Simple<'src, char>>> {
    just("true")
        .to(true)
        .or(just("false").to(false))
        .map(|b| Expr::Atom(Atom::Bool(b)))
}

#[test]
fn test_boolean_parser() {
    assert_eq!(boolean().parse("true"), Ok(Expr::Atom(Atom::Bool(true))));
    assert_eq!(boolean().parse("false"), Ok(Expr::Atom(Atom::Bool(false))));
    assert!(boolean().parse("tru").is_err());
    assert!(boolean().parse("False").is_err());
}


/// Parses floating point and integer numbers and returns them as [`Expression::Atom(Atom::Double(...))`]
/// or [`Expression::Atom(Atom::Int(...))`] types. The following formats are supported:
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
fn numbers<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Simple<'a, char>>> {
    let digits = text::digits(10).slice();

    let frac = just('.').then(digits);

    let exp = just('e')
        .or(just('E'))
        .then(one_of("+-").or_not())
        .then(digits);

    let floating = just('-')
        .or_not()
        .then(text::int(10))
        .then(frac.or_not())
        .then(exp.or_not())
        .map_slice(|s: &str| {
            Expr::Atom(Atom::Double(s.parse().unwrap()))
        });

    let unsigned_integer = text::int(10)
        .then_ignore(just('u'))
        .map(|s: &str| Expr::Atom(Atom::UInt(s.parse().unwrap())));
    let integer = text::int(10)
        .map(|s: &str| Expr::Atom(Atom::Int(s.parse().unwrap())));

    choice((unsigned_integer, floating, integer))
        .padded()
}

#[test]
fn test_number_parser_unsigned_numbers() {
    //let unsigned_integer = text::int::<char, Simple<char>>(10).then_ignore(just('u')).map(|s: String| Expr::Atom(Atom::UInt(s.as_str().parse().unwrap())));
    //assert_eq!(unsigned_integer.parse("1u"), Ok(Expr::Atom(Atom::UInt(1))));
    assert_eq!(numbers().parse("1u"), Ok(Expr::Atom(Atom::UInt(1))));
    assert_eq!(numbers().parse("1up"), Ok(Expr::Atom(Atom::UInt(1))));
}

#[test]
fn test_number_parser_int() {
    assert_eq!(numbers().parse("1"), Ok(Expr::Atom(Atom::Int(1))));

    // Debatable if this should be allowed. Ref CEL Spec:
    // https://github.com/google/cel-spec/blob/master/doc/langdef.md#numeric-values
    // "negative integers are produced by the unary negation operator"
    assert_eq!(numbers().parse("-100"), Ok(Expr::Atom(Atom::Int(-100))));
}

#[test]
fn test_number_parser_double() {
    assert_eq!(numbers().parse("1e3"), Ok(Expr::Atom(Atom::Double(1000.0))));
    assert_eq!(
        numbers().parse("-1e-3"),
        Ok(Expr::Atom(Atom::Double(-0.001)))
    );
    assert_eq!(
        numbers().parse("-1.4e-3"),
        Ok(Expr::Atom(Atom::Double(-0.0014)))
    );
}

fn str_inner<'src>(
    delimiter: &str,
    escaping: bool,
) -> impl Parser<&'src str, String, extra::Err<Rich<'src, char>>> + '_ {
    let unicode = any().filter(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(4)
        .collect::<String>()
        .validate(|digits, span, emitter| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emitter.emit(Rich::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let hex_code_point = any().filter(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(2)
        .collect::<String>()
        .validate(|digits, span, emitter| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emitter.emit(Rich::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let octal_code_point = any().filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(3)
        .collect::<String>()
        .validate(|digits, span, emitter| {
            char::from_u32(u32::from_str_radix(&digits, 8).unwrap()).unwrap_or_else(|| {
                emitter.emit(Rich::custom(span, "invalid unicode character"));
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
    ))).boxed();

    let mut forbidden = just(delimiter).boxed();
    let mut inner_string = none_of(delimiter).boxed();

    if escaping {
        forbidden = just(delimiter).or(just("\\")).boxed();
        inner_string = escape
            .and_is(forbidden.not())
            .boxed();
    }

    inner_string
        .repeated()
        .delimited_by(just(delimiter), just(delimiter))
        .collect::<String>()
}

// Ref https://github.com/01mf02/jaq/blob/main/jaq-parse/src/token.rs
// See also https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295-L354
// A parser for strings; adapted from Chumsky's JSON example parser.
fn str_<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
    let single_quoted_string = str_inner("'", true);

    let double_quoted_string = str_inner("\"", true);

    // Raw strings don't interpret escape sequences.

    let single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'", false));

    let double_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("\"", false));

    let triple_single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'''", false));

    let triple_single_quoted_escaped_string =
        str_inner("'''", true);

    let triple_double_quoted_string = str_inner("\"\"\"", true);

    choice((
        triple_single_quoted_raw_string,
        triple_single_quoted_escaped_string,
        triple_double_quoted_string,
        single_quoted_raw_string,
        single_quoted_string,
        double_quoted_raw_string,
        double_quoted_string,
    ))
    .map(|s| Expr::Atom(Atom::String(s.into())))


    //any().map(|s| Expr::Atom(Atom::Bool(true)))
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
        Ok(Expr::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse("\"Hello!\""),
        Ok(Expr::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse("'\n'"),
        Ok(Expr::Atom(Atom::String(String::from("\n").into())))
    );
    assert_eq!(
        str_().parse(r"'\n'"),
        Ok(Expr::Atom(Atom::String(String::from("\n").into())))
    );

    assert_eq!(
        str_().parse(r"'''hello'''"),
        Ok(Expr::Atom(Atom::String(String::from("hello").into())))
    );
    // Check triple quoted strings interpret escape sequences (note this is a rust raw string, not a CEL raw string)
    assert_eq!(
        str_().parse(r"'''\n'''"),
        Ok(Expr::Atom(Atom::String(String::from("\n").into())))
    );
}

#[test]
fn test_raw_str_parser() {
    assert_eq!(
        str_().parse(r"r'\n'"),
        Ok(Expr::Atom(Atom::String(String::from("\\n").into())))
    );
    assert_eq!(
        str_().parse(r"R'\n'"),
        Ok(Expr::Atom(Atom::String(String::from("\\n").into())))
    );
    assert_eq!(
        str_().parse("r'1'"),
        Ok(Expr::Atom(Atom::String(String::from("1").into())))
    );
    assert_eq!(
        str_().parse("r\"Hello!\""),
        Ok(Expr::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse("R\"Hello!\""),
        Ok(Expr::Atom(Atom::String(String::from("Hello!").into())))
    );
    assert_eq!(
        str_().parse(r"r'''hello'''"),
        Ok(Expr::Atom(Atom::String(String::from("hello").into())))
    );
    assert_eq!(
        str_().parse(r"r'''\n'''"),
        Ok(Expr::Atom(Atom::String(String::from("\\n").into())))
    );
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
    let ident = text::ident()
        .padded()
        .map(|identifier: &str| Expr::Var(identifier.into()));

    let null = just("null")
        .padded()
        .map(|_| Expr::Atom(Atom::Null));

    let expr = recursive(|expr| {
        let literal = choice((numbers(), boolean(), str_(), null));

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
            .map(|(name, args)| Expr::Member(Box::new(name), MemberOp::Call(args)))
            .padded();

        // TODO support "a.b[0]"
        // attribute access should a recursive "member" expression
        let member = ident.clone();

        let attribute_access = ident
            .clone()
            .foldl(
                just('.').ignore_then(member.clone()).repeated().at_least(1),
                |lhs, rhs| match rhs {
                    Expr::Var(v) => Expr::Member(Box::new(lhs), MemberOp::Attribute(v)),
                    _ => panic!("Expected identifier after '.'"),
                }, );

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
            .delimited_by(just('['), just(']'));

        let index_access = ident
            .foldl(
                index.repeated().at_least(1),
                |lhs, rhs| Expr::Member(Box::new(lhs), MemberOp::Index(rhs)));

        let list = expr_list
            .clone()
            // Ignore trailing comma
            .delimited_by(just('['), just(']'))
            .map(|items| Expr::List(items));

        let map_item = expr
            .clone()
            .then_ignore(just(':'))
            .then(expr.clone())
            .padded();

        let map = map_item
            .clone()
            .separated_by(just(','))
            .delimited_by(just('{'), just('}'))
            .padded()
            .collect()
            .map(|items| Expr::Map(items));

        let primary = function_call
            .or(attribute_access)
            .or(index_access)
            .or(literal)
            .or(expr
                .clone()
                .delimited_by(just('('), just(')'))
            )
            .or(list)
            .or(map)
            .or(ident)
            .padded()
            .boxed();

        let op = |c| just(c).padded();

        let not = op('!')
            .ignore_then(primary.clone())
            .map(|rhs| Expr::Unary(UnaryOp::Not, Box::new(rhs)));

        let negation = op('-')
            .ignore_then(primary.clone())
            .map(|rhs| Expr::Unary(UnaryOp::Neg, Box::new(rhs)));

        let unary = choice((not, negation))
            .or(primary)
            .padded()
            .boxed();

        let product_div_op = op('*').to(BinaryOp::Mul).or(op('/').to(BinaryOp::Div));

        let multiplication = unary
            .clone()
            .foldl(
                product_div_op.then(unary.clone()).repeated(),
                |lhs, (binary_op, rhs)| Expr::Binary(Box::new(lhs), binary_op, Box::new(rhs)))
            .boxed();

        let sum_sub_op = op('+').to(BinaryOp::Add).or(op('-').to(BinaryOp::Sub));

        let addition = multiplication
            .clone()
            .foldl(
                sum_sub_op.then(multiplication.clone()).repeated(),
                |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
            .boxed();

        let relationship_op = just("==")
            .to(BinaryOp::Equals)
            .or(just("!=").to(BinaryOp::NotEquals))
            .or(just(">=").to(BinaryOp::GreaterThanOrEqual))
            .or(just("<=").to(BinaryOp::LessThanOrEqual))
            .or(just('>').to(BinaryOp::GreaterThan))
            .or(just('<').to(BinaryOp::LessThan));

        let relation = addition
            .clone()
            .foldl(
                relationship_op.then(addition.clone()).repeated(),
                |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
            .boxed();

        relation
    });

    expr.then_ignore(end())
}

#[test]
fn test_parser_bool() {
    assert_eq!(parser().parse("true"), Ok(Expr::Atom(Atom::Bool(true))));
    assert_eq!(parser().parse("false"), Ok(Expr::Atom(Atom::Bool(false))));
    assert_eq!(
        parser().parse("!false"),
        Ok(Expr::Unary(
            UnaryOp::Not,
            Box::new(Expr::Atom(Atom::Bool(false)))
        ))
    );
    assert_eq!(
        parser().parse("!true"),
        Ok(Expr::Unary(
            UnaryOp::Not,
            Box::new(Expr::Atom(Atom::Bool(true)))
        ))
    );
}

#[test]
fn test_parser_binary_bool_expressions() {
    assert_eq!(
        parser().parse("true == true"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Bool(true))),
            BinaryOp::Equals,
            Box::new(Expr::Atom(Atom::Bool(true)))
        ))
    );
}

#[test]
fn test_parser_str() {
    assert_eq!(
        parser().parse("'hi'"),
        Ok(Expr::Atom(Atom::String(String::from("hi").into())))
    );
    assert_eq!(
        parser().parse("'true'"),
        Ok(Expr::Atom(Atom::String(String::from("true").into())))
    );

    assert_eq!(
        parser().parse("'''true\n'''"),
        Ok(Expr::Atom(Atom::String(String::from("true\n").into())))
    );
    assert_eq!(
        parser().parse(r##""""He said "Hi I'm Brian".""""##),
        Ok(Expr::Atom(Atom::String(
            String::from("He said \"Hi I'm Brian\".").into()
        )))
    );
}

#[test]
fn test_parser_raw_strings() {
    assert_eq!(
        parser().parse("r'\n'"),
        Ok(Expr::Atom(Atom::String(String::from("\n").into())))
    );
}

#[test]
fn test_parser_positive_numbers() {
    assert_eq!(parser().parse("1"), Ok(Expr::Atom(Atom::Int(1))));
    assert_eq!(parser().parse("1u"), Ok(Expr::Atom(Atom::UInt(1))));
    assert_eq!(parser().parse("1.0"), Ok(Expr::Atom(Atom::Double(1.0))));
}

#[test]
fn test_parser_negative_numbers() {
    assert_eq!(
        parser().parse("-1"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::Int(1)))
        ))
    );
    assert_eq!(
        parser().parse("-1u"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::UInt(1)))
        ))
    );
    assert_eq!(
        parser().parse("-1e3"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::Double(1000.0)))
        ))
    );
    assert_eq!(
        parser().parse("-1e-3"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::Double(0.001)))
        ))
    );
    assert_eq!(
        parser().parse("-1.4e-3"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::Double(0.0014)))
        ))
    );
}

#[test]
fn test_parser_delimited_expressions() {
    assert_eq!(
        parser().parse("(-((1)))"),
        Ok(Expr::Unary(
            UnaryOp::Neg,
            Box::new(Expr::Atom(Atom::Int(1)))
        ))
    );
}

#[test]
fn test_parser_binary_product_expressions() {
    assert_eq!(
        parser().parse("2 * 3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Mul,
            Box::new(Expr::Atom(Atom::Int(3)))
        ))
    );
    assert_eq!(
        parser().parse("2 * -3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Mul,
            Box::new(Expr::Unary(
                UnaryOp::Neg,
                Box::new(Expr::Atom(Atom::Int(3)))
            ))
        ))
    );

    assert_eq!(
        parser().parse("2 / -3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Div,
            Box::new(Expr::Unary(
                UnaryOp::Neg,
                Box::new(Expr::Atom(Atom::Int(3)))
            ))
        ))
    );
}

#[test]
fn test_parser_sum_expressions() {
    assert_eq!(
        parser().parse("2 + 3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Add,
            Box::new(Expr::Atom(Atom::Int(3)))
        ))
    );
    assert_eq!(
        parser().parse("2 - -3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Sub,
            Box::new(Expr::Unary(
                UnaryOp::Neg,
                Box::new(Expr::Atom(Atom::Int(3)))
            ))
        ))
    );
}
