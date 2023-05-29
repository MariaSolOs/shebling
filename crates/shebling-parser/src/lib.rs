mod error;
mod parser;
mod span;

pub fn parse(_source_code: &str) {
    // HACK: When reporting errors, add a newline to the end of the source
    // so that miette can highlight the last character.
}
