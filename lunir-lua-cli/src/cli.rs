use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(version)]
pub(crate) struct Cli {
    pub(crate) file: PathBuf,
}
