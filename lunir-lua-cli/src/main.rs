mod cli;

use std::{
    fs::{metadata, File},
    io::{self, ErrorKind, Read},
};

use clap::Parser;
use cli::Cli;
use lunir_lua50::frontend::lexer::lex;

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    let mut path = cli.file;

    path = path.canonicalize()?;

    if !path.is_file() {
        return io::Result::Err(io::Error::new(
            ErrorKind::Other,
            format!("Provided path {path:?} does not point to a file!"),
        ));
    }

    let len = metadata(path.clone())?.len();

    let mut buf = String::with_capacity(len as usize);

    let mut file = File::open(path)?;

    file.read_to_string(&mut buf)?;

    lex(&buf).iter().for_each(|t| println!("{t:?}"));

    Ok(())
}
