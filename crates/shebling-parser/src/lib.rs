mod error;
mod parser;
mod span;

// TODO: Return the AST here instead of just printing.
pub fn parse(source_code: &str, file_path: &str) {
    parser::parse(source_code, file_path);
}
