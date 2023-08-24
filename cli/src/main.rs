use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::Read;
use std::rc::Rc;

use cel_interpreter::types::{CelMap, CelMapKey, CelType, NumericCelType};
use cel_interpreter::{eval, map_json_text_to_cel_types};
use cel_parser::parse_cel_expression;
use clap::Parser as ClapParser;

#[derive(ClapParser)]
#[command(name = "cel")]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
struct Cli {
    /// Optional json context file to operate on
    #[arg(short, long, value_name = "FILE")]
    input: Option<String>,

    #[arg(long)]
    expression: String,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
}

fn main() {
    let app = Cli::parse();

    // Read the context (from stdin or a JSON file)

    // Note the as_deref maps the app.input's Option<String> to Option<str>
    let context_filename: Option<&str> = app.input.as_deref();

    let context = context_filename
        .map(|filename| load_context(filename).expect("Error loading context file"))
        .map(|json_data| {
            // Map the json string of context into CelType::Map
            let context_result: Result<CelType, String> = map_json_text_to_cel_types(&json_data);
            context_result.expect("Error parsing context data")
        })
        .unwrap_or_else(|| _create_empty_cel_map());

    let src = app.expression.to_string();

    //println!("Context: {:?}", context);
    //println!("CEL Expression:\n{:?}\n", src);

    match parse_cel_expression(src) {
        Ok(ast) => {
            println!("AST: \n{:?}\n", ast);
            println!("Evaluating program");
            let default_vars = &mut Vec::new();

            // Pre-populate the default vars context
            // Create hard coded variables for testing, and eventually builtin macros
            default_vars.push((
                String::from("test_int"),
                CelType::NumericCelType(NumericCelType::Int(1)),
            ));
            default_vars.push((
                String::from("test_str"),
                CelType::String(Rc::new(String::from("hello world"))),
            ));
            let mut test_map = HashMap::new();
            test_map.insert(
                CelMapKey::Int(1),
                CelType::String(Rc::new(String::from("one"))),
            );
            test_map.insert(
                CelMapKey::String(Rc::new(String::from("two"))),
                CelType::NumericCelType(NumericCelType::Int(2)),
            );
            default_vars.push((
                String::from("test_map"),
                CelType::Map(CelMap {
                    map: Rc::new(test_map),
                }),
            ));

            // Now add the optional context passed in by the user.
            // eg context might be
            // Map(CelMap { map: {String("float key"): NumericCelType(Float(3.14)), String("array key"): List([Map(CelMap { map: {String("a bool"): Bool(true)} })]), String("nested object"): Map(CelMap { map: {String("nested key"): NumericCelType(Int(1))} }), String("foo"): String("bar"), String("int key"): NumericCelType(Int(42)), String("string key"): String("my value")} })
            // and we want to add that to the default_vars Vec
            if let CelType::Map(CelMap { map }) = context {
                map.iter().for_each(|(key, value)| {
                    default_vars.push((String::from(key), value.clone()));
                })
            }

            match eval(&ast, default_vars) {
                Ok(output) => println!("{:?}", output),
                Err(eval_err) => println!("Evaluation error: {}", eval_err),
            }
        }
        Err(parse_errs) => {
            println!("An error occurred during parsing");
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {:?}", e));
        }
    }
}

fn _create_empty_cel_map() -> CelType {
    CelType::Map(CelMap {
        map: Rc::new(HashMap::new()),
    })
}

/// Load data from a file or stdin
fn load_context(context_filename: &str) -> Result<String, String> {
    match context_filename {
        "-" => {
            println!("Reading context from stdin");
            let stdin = io::stdin();
            let mut reader = stdin.lock();
            let mut data = String::new();
            reader.read_to_string(&mut data).unwrap();
            Ok(data)
        }
        input_filename => fs::read_to_string(input_filename)
            .map_err(|e| format!("Error reading context file: {}", e)),
    }
}
