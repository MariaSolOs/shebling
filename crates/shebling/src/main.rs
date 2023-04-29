use clap::{error::ErrorKind, CommandFactory, Parser};
use std::{fs, path::PathBuf};

// TODO: Default values?
#[derive(Parser)]
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

    // TODO: Use miette here.
    match fs::read_to_string(&args.path) {
        Ok(source_code) => shebling_parser::parse(&source_code),
        Err(err) => {
            let mut cmd = Args::command();
            cmd.error(
                ErrorKind::Io,
                format!("{} - {}", args.path.to_string_lossy(), err),
            )
            .exit();
        }
    }
}
