mod ast;
mod parser;
mod shell;

pub fn run(source: &str) {
    parser::parse(source);
}
