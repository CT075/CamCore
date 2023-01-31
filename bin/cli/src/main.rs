use std::path::PathBuf;

use clap::{ArgAction, Parser, ValueEnum};

mod errors;

#[derive(Debug, Clone, ValueEnum)]
enum Phase {
    ///
    Lexing,
    ///
    Preprocessing,
}

#[derive(Debug, Parser)]
#[command(name = "CamCore")]
#[command(author = "CT075 <cam@camdar.io>")]
#[command(version)]
#[command(bin_name = "camcore")]
#[command(about = "An Event Assembler", long_about = None)]
struct Cli {
    /// The list of files to assemble.
    inputs: Vec<String>,
    /// The game to assemble for. Case-insensitive.
    ///
    /// Internally, `--game=MyGame` implies `-D_MYGAME_ -D__GAME__=MyGame`. It
    /// is also used by the raws engine to restrict which raws are available.
    #[arg(short, long)]
    game: Option<String>,
    /// Sets the verbosity level. Can be repeated up to 3 times, e.g. -vvv.
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
    /// Output debug information. Implies -vvv.
    #[arg(long)]
    debug: bool,
    /// Suppress all non-error messages. Mutually exclusive with --verbose.
    #[arg(short, long)]
    quiet: bool,
    /// Display build times after assembly.
    #[arg(long)]
    build_times: bool,
    /// Output defines and top-level symbols as a no$-compatible .sym file.
    #[arg(long)]
    nocash_sym: Option<PathBuf>,
    /// Display colored error output. Default true.
    #[arg(long, default_value_t = true)]
    color: bool,
    /// Disables safeguards on the differences between this program's interface
    /// and NLCore/ColorzCore.
    #[arg(long)]
    no_legacy_cli_checks: bool,
    /// Predefine macros or definitions
    #[arg(short = 'D', long)]
    defines: Vec<String>,
    /// Stop assembly after the given phase
    #[arg(long)]
    stop_after: Phase,
}

fn main() {
    let cli = Cli::parse();

    println!("{:?}\n", cli);
}
