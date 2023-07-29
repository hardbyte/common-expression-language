pub mod ast;
pub mod parser;
use chumsky::error::{Rich};
use chumsky::Parser;

pub fn parse_cel_expression<'src>(input: String) -> Result<ast::Expr, Vec<Rich<'src, char>>> {
    // Expose the internal parser::parse function
    parser::parser().parse(input.as_str()).into_result()
}
