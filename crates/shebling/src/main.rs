use clap::{CommandFactory, Parser};
use std::{fs, path::PathBuf};

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
        Ok(source) => shebling_parser::parse(&source),
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
