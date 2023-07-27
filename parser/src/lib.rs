
pub mod ast;
pub mod parser;
use chumsky::Parser;
use chumsky::error::Simple;


pub fn parse_cel_expression(input: String) -> Result<ast::Expr, Vec<Simple<char>>> {
    // Expose the internal parser::parse function
    parser::parser().parse(input)
}
