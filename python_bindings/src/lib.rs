use cel_interpreter::types::{CelType, NumericCelType};
use cel_interpreter::{eval, map_json_text_to_cel_types};
use cel_parser::ast::Expression;
use cel_parser::parse_cel_expression;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::AsPyPointer;
use std::fmt::format;

#[derive(Debug)]
struct RustyCelType(CelType);

impl IntoPy<PyObject> for RustyCelType {
    fn into_py(self, py: Python<'_>) -> PyObject {
        // Just use the native rust type's existing
        // IntoPy implementation
        match self {
            RustyCelType(CelType::Bool(b)) => b.into_py(py),
            RustyCelType(CelType::Null) => None::<bool>.into_py(py),
            RustyCelType(CelType::String(s)) => s.as_ref().into_py(py),
            RustyCelType(CelType::NumericCelType(n)) => match n {
                NumericCelType::Int(x) => x.into_py(py),
                NumericCelType::UInt(x) => x.into_py(py),
                NumericCelType::Float(x) => x.into_py(py),
            },
            //RustyCelType(CelType::Bytes(bytes)) => bytes.into_py(py),
            //RustyCelType(CelType::List(xs)) => xs.into_py(py),
            //RustyCelType(CelType::Map(d)) => d.into_py(py),
            // For now just turn everything else into a String:
            nonprimitive => format!("{:?}", nonprimitive).into_py(py),
        }
    }
}

/// Evaluate a CEL expression
/// Returns a String representation of the result
#[pyfunction]
fn evaluate(src: String) -> PyResult<RustyCelType> {
    match parse_cel_expression(src) {
        Err(parse_errs) => {
            println!("An error occurred during parsing");
            parse_errs
                .into_iter()
                .for_each(|e| println!("Parse error: {:?}", e));
            Err(PyValueError::new_err("Parse Error"))
        }
        Ok(ast) => {
            let default_vars = &mut Vec::new();
            match eval(&ast, default_vars) {
                Ok(output) => Ok(RustyCelType(output)),
                Err(eval_err) => Err(PyValueError::new_err(format!(
                    "Evaluation error: {}",
                    eval_err
                ))),
            }
        }
    }
}

/// The rustycel Python module
#[pymodule]
fn rustycel(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(evaluate, m)?)?;
    Ok(())
}
