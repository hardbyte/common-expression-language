use cel_parser::parse_cel_expression;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use cel_interpreter::{eval, map_json_text_to_cel_types};
use cel_parser::ast::Expression;

fn parse(input: &str) -> Expression {
    crate::parse_cel_expression(black_box(input.to_string()))
        .unwrap_or_else(|_e| panic!("Parse error"))
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse-atoms");

    group.bench_function("parse_string_single_quotes", |b| {
        b.iter(|| parse("'hello world'"))
    });
    group.bench_function("parse_raw_string_single_quotes", |b| {
        b.iter(|| parse("r'hello'"))
    });

    group.bench_function("parse_int_1", |b| b.iter(|| parse("0")));
    group.bench_function("parse_int_2", |b| b.iter(|| parse("1565")));
    group.bench_function("parse_int_3", |b| b.iter(|| parse("-78912")));

    group.bench_function("parse_float_1", |b| b.iter(|| parse("2.0005")));
    group.bench_function("parse_float_2", |b| b.iter(|| parse("0.0000005")));
    group.bench_function("parse_float_3", |b| b.iter(|| parse("1.34e5")));

    group.bench_function("parse_empty_list", |b| b.iter(|| parse("[]")));
    group.bench_function("parse_empty_map", |b| b.iter(|| parse("{}")));
    group.finish();

    let mut group2 = c.benchmark_group("parse-objects");
    group2.bench_function("parse_list", |b| b.iter(|| parse("[1, 2, 3]")));
    group2.bench_function("parse_map", |b| {
        b.iter(|| parse("{'a': 1, 'b': 2, 'c': 3}"))
    });

    group2.bench_function("parse_integer_ne_relation", |b| b.iter(|| parse("2 != 3")));
    group2.bench_function("parse_integer_e_relation", |b| b.iter(|| parse("2 == 3")));

    group2.bench_function("parse_map_mixed_types", |b| {
        b.iter(|| parse("{'a': 'hello', 'b': [1.0, 2.0], 'c': 3u}"))
    });

    group2.bench_function("parse_map_list_member", |b| {
        b.iter(|| parse("{'a': 'hello', 'b': [1.0, 2.0], 'c': 3u}['b'][1]"))
    });

    group2.bench_function("parse_ternary", |b| {
        b.iter(|| parse("false ? 'result_true' : 'result_false'"))
    });
    group2.finish();

    // Benchmark for evaluating expressions using the cel_interpreter::eval function
    let mut eval_group = c.benchmark_group("eval-expressions");
    // Benchmark for evaluating different types of expressions
    eval_group.bench_function("eval_simple_arithmetic", |b| {
        b.iter(|| {
            let expr = parse("1 + 2");
            let default_vars = &mut Vec::new();
            eval(&expr, default_vars).unwrap();
        })
    });
    eval_group.bench_function("eval_logical_expression", |b| {
        b.iter(|| {
            let expr = parse("true && false");
            let default_vars = &mut Vec::new();
            eval(&expr, default_vars).unwrap();
        })
    });
    eval_group.bench_function("eval_function_call", |b| {
        b.iter(|| {
            let expr = parse("size('hello')");
            let default_vars = &mut Vec::new();
            eval(&expr, default_vars).unwrap();
        })
    });
    // Ensure benchmarks cover both successful evaluations and error scenarios
    eval_group.bench_function("eval_error_scenario", |b| {
        b.iter(|| {
            let expr = parse("undefined_var");
            let default_vars = &mut Vec::new();
            assert!(eval(&expr, default_vars).is_err());
        })
    });
    // Use realistic CEL expressions that might be encountered in real-world usage
    eval_group.bench_function("eval_realistic_expression", |b| {
        b.iter(|| {
            let expr = parse("{'a': 1, 'b': 2}['a'] + 3");
            let default_vars = &mut Vec::new();
            eval(&expr, default_vars).unwrap();
        })
    });
    eval_group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
