use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::Read;
use std::rc::Rc;

use cel_interpreter::eval;
use cel_interpreter::types::{CelMap, CelMapKey, CelType, NumericCelType};
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
    //let src = std::env::args().nth(1).unwrap();
    let app = Cli::parse();

    let src = app.expression.to_string();
    // Read the context (a JSON string or file)
    // Note the as_deref maps the Option<String> to Option<str>
    let data: Option<String> = match app.input.as_deref() {
        Some("-") => {
            // read from stdin
            println!("Reading context from stdin");
            let stdin = io::stdin();
            let mut reader = stdin.lock();
            let mut data = String::new();
            reader.read_to_string(&mut data).unwrap();
            println!("Context file read");
            Some(data)
        }
        Some(input_filename) => {
            // Read the entire file into a string
            match fs::read_to_string(input_filename) {
                Ok(data) => {
                    println!("Context file read");
                    Some(data)
                }
                Err(e) => {
                    println!("Error reading context file: {}", e);
                    None
                }
            }
        }
        // No input file specified. That's ok just return the None variant
        None => None,
    };

    println!("Loaded source. {:?}", src);
    println!("Loading context data:\n{:?}", data);

    match parse_cel_expression(src) {
        Ok(ast) => {
            println!("AST: \n{:?}\n", ast);
            println!("Evaluating program");
            let default_vars = &mut Vec::new();

            // Create hard coded variables for testing, and eventually builtin macros
            let test_int_var_name = String::from("test_int");
            let test_str_var_name = String::from("test_str");
            let test_map_var_name = String::from("test_map");
            default_vars.push((
                &test_int_var_name,
                CelType::NumericCelType(NumericCelType::Int(1)),
            ));
            default_vars.push((
                &test_str_var_name,
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
                &test_map_var_name,
                CelType::Map(CelMap {
                    map: Rc::new(test_map),
                }),
            ));

            match eval(&ast, default_vars) {
                Ok(output) => println!("{:?}", output),
                Err(eval_err) => println!("Evaluation error: {}", eval_err),
            }
        }
        Err(parse_errs) => {
            println!("An error occurred!");
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {:?}", e));
        }
    }
}
