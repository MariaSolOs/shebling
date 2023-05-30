mod error;
mod parser;
mod span;

pub fn parse(source_code: &str, file_path: &str) {
    parser::parse(source_code, file_path);
}
