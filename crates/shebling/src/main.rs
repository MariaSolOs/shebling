mod lexer;

use clap::{CommandFactory, Parser};
use std::{fs, path::PathBuf, sync::Arc};

use lexer::Lexer;

// TODO: Default values?
#[derive(clap::Parser)]
#[command(
    author,
    version,
    about,
    help_template = "{name}
{tab}{about-with-newline}
{tab}{author-with-newline}
{usage-heading} {usage}

{all-args}"
)]
struct Args {
    // TODO: Accept globs? Folders?
    /// Path to the file to lint.
    #[arg(short, long)]
    path: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file_path = args.path.to_string_lossy();
    match fs::read_to_string(&args.path) {
        Ok(source) => {
            let lexer = Lexer::new(&source);
            let (tokens, errors) = lexer.tokenize();
            println!("TOKENS: {:#?}", tokens);

            // HACK: When reporting errors, we add a newline to the end of the source
            // so that miette can highlight the last character.
            let source = Arc::new(miette::NamedSource::new(
                file_path,
                source.to_owned() + "\n",
            ));

            errors.into_iter().for_each(|err| {
                println!(
                    "{:?}",
                    miette::Report::new(err).with_source_code(Arc::clone(&source))
                );
            });
        }
        Err(err) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::Io,
                format!("{} - {}", file_path, err),
            )
            .exit();
        }
    }
}
