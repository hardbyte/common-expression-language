use crate::ast::Expr::Unary;
use crate::ast::{Atom, BinaryOp, Expr, UnaryOp};

use chumsky::prelude::*;
use chumsky::Parser;

fn boolean() -> impl Parser<char, Expr, Error = Simple<char>> {
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
fn numbers() -> impl Parser<char, Expr, Error = Simple<char>> {
    let digits = text::digits::<char, Simple<char>>(10);

    let frac = just('.').chain::<char, _, _>(digits.clone().or_not());

    let exp = just('e')
        .or(just('E'))
        .chain::<char, _, _>(one_of("+-").or_not())
        .chain::<char, _, _>(digits.clone());

    let floating = just('-')
        .or_not()
        .chain::<char, _, _>(text::int::<char, Simple<char>>(10))
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .try_map(|chars, span| {
            let str = chars.into_iter().collect::<String>();

            if let Ok(i) = str.parse::<i64>() {
                Ok(Expr::Atom(Atom::Int(i)))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Expr::Atom(Atom::Double(f)))
            } else {
                Err(Simple::expected_input_found(span, None, None))
            }
        });

    let unsigned_integer = text::int::<char, Simple<char>>(10)
        .then_ignore(just('u'))
        .map(|s: String| Expr::Atom(Atom::UInt(s.as_str().parse().unwrap())));
    let integer = text::int::<char, Simple<char>>(10)
        .map(|s: String| Expr::Atom(Atom::Int(s.as_str().parse().unwrap())));

    choice((unsigned_integer, floating, integer))
        .padded()
        .labelled("number")
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

// Ref https://github.com/01mf02/jaq/blob/main/jaq-parse/src/token.rs
// See also https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295-L354
// A parser for strings; adapted from Chumsky's JSON example parser.
fn str_() -> impl Parser<char, Expr, Error = Simple<char>> {
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
    )));

    let single_quoted_string = just('\'')
        .ignore_then(filter(|c| *c != '\\' && *c != '\'').or(escape).repeated())
        .then_ignore(just('\''))
        .collect::<String>()
        .labelled("string");

    // TODO
    // let triple_single_quoted_string = just('\'').then(just('\'')).then(just('\''))
    //     .ignore_then(filter(|c| *c != '\\').or(escape).repeated())
    //     .then_ignore(just('\'').then(just('\'')).then(just('\'')))
    //     .collect::<String>()
    //     .labelled("string");

    let double_quoted_string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string");

    choice((single_quoted_string, double_quoted_string, triple_single_quoted_string))
        .map(|s| Expr::Atom(Atom::String(s.into())))

}

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident::<char, Simple<char>>()
        .padded()
        .labelled("identifier");

    let expr = recursive(|expr| {
        let literal = choice((numbers(), boolean()));

        let atomic_expression = literal
            .or(expr.clone().delimited_by(just('('), just(')')))
            .or(ident.map(Expr::Var))
            .or(str_())
            .padded()
            .boxed();

        let op = |c| just::<char, _, Simple<char>>(c).padded();

        let not = op('!')
            .ignore_then(atomic_expression.clone())
            .map(|rhs| Expr::Unary(UnaryOp::Not, Box::new(rhs)));

        let negation = op('-')
            .ignore_then(atomic_expression.clone())
            .map(|rhs| Expr::Unary(UnaryOp::Neg, Box::new(rhs)))
            .labelled("negation");

        let unary = choice((not, negation))
            .or(atomic_expression)
            .padded()
            .boxed();

        let product_div_op = op('*').to(BinaryOp::Mul).or(op('/').to(BinaryOp::Div));

        let product = unary
            .clone()
            .then(product_div_op.then(unary.clone()).repeated()) // Could have a repeated here?
            .foldl(|lhs, (binary_op, rhs)| Expr::Binary(Box::new(lhs), binary_op, Box::new(rhs)))
            .labelled("product_or_division")
            .boxed();

        let sum_sub_op = op('+').to(BinaryOp::Add).or(op('-').to(BinaryOp::Sub));

        let sum = product
            .clone()
            .then(sum_sub_op.then(product.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
            .labelled("sub_or_sub")
            .boxed();
        sum
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

#[test]
fn test_parser_bool() {
    assert_eq!(parser().parse("true"), Ok(Expr::Atom(Atom::Bool(true))));
    assert_eq!(parser().parse("false"), Ok(Expr::Atom(Atom::Bool(false))));
    assert_eq!(
        parser().parse("!false"),
        Ok(Unary(UnaryOp::Not, Box::new(Expr::Atom(Atom::Bool(false)))))
    );
    assert_eq!(
        parser().parse("!true"),
        Ok(Unary(UnaryOp::Not, Box::new(Expr::Atom(Atom::Bool(true)))))
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
            Box::new(Unary(UnaryOp::Neg, Box::new(Expr::Atom(Atom::Int(3)))))
        ))
    );

    assert_eq!(
        parser().parse("2 / -3"),
        Ok(Expr::Binary(
            Box::new(Expr::Atom(Atom::Int(2))),
            BinaryOp::Div,
            Box::new(Unary(UnaryOp::Neg, Box::new(Expr::Atom(Atom::Int(3)))))
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
            Box::new(Unary(UnaryOp::Neg, Box::new(Expr::Atom(Atom::Int(3)))))
        ))
    );
}
