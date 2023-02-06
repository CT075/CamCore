use std::{collections::HashMap, fmt::Display, fs, process};

use clap::{Parser, ValueEnum};
use colored::Colorize;

use ea::{io::FileContents, plumbing::*};

mod errors;

static KNOWN_FE_GAMES: [&'static str; 3] = ["FE6", "FE7", "FE8"];

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
    /// The list of files to assemble. Files will be read in the order given.
    inputs: Vec<String>,
    /// The game to assemble for. Case-insensitive.
    ///
    /// Internally, `--game=MyGame` implies `-D_MYGAME_ -D__GAME__=MyGame`. It
    /// is also used by the raws engine to restrict which raws are available.
    #[arg(short, long)]
    game: Option<String>,
    // /// Sets the verbosity level. Can be repeated up to 3 times, e.g. -vvv.
    //#[arg(short, long, action = ArgAction::Count)]
    //verbose: u8,
    // /// Output debug information. Implies -vvv.
    //#[arg(long)]
    //debug: bool,
    // /// Suppress all non-error messages. Mutually exclusive with --verbose.
    //#[arg(short, long)]
    //quiet: bool,
    // /// Display build times after assembly.
    //#[arg(long)]
    //build_times: bool,
    // /// Output defines and top-level symbols as a no$-compatible .sym file.
    //#[arg(long)]
    //nocash_sym: Option<PathBuf>,
    /// Display colored error output. Default true.
    #[arg(long, default_value_t = true)]
    color: bool,
    /// Disables legacy cli mode.
    #[arg(long)]
    no_legacy_cli: bool,
    // /// Predefine macros or definitions
    //#[arg(short = 'D', long)]
    //defines: Vec<String>,
    /// Stop assembly after the given phase
    #[arg(long)]
    stop_after: Option<Phase>,
}

struct CliError {
    short: String,
    // TODO: nonempty vec
    hints: Option<Vec<String>>,
}

fn display_cli_error_and_exit(err: CliError) -> ! {
    print!("{} {}\n", "error:".red(), err.short);
    match err.hints {
        None => (),
        Some(hints) => {
            print!("\n");
            for hint in hints.into_iter() {
                print!("{}\n", hint)
            }
        }
    }
    process::exit(1)
}

fn short_error(s: impl Display) -> CliError {
    CliError {
        short: format!("{}", s),
        hints: None,
    }
}

fn display_short_error_and_exit(s: impl Display) -> ! {
    display_cli_error_and_exit(short_error(s))
}

// TODO: move this to [ea::engine::io::util] or something
fn load_file(path: String) -> Result<FileContents<String>, CliError> {
    let path1 = path.clone();
    let path2 = path.clone();
    fs::read_to_string(path)
        .map(move |c| FileContents::new(path1, c))
        .map_err(move |e| {
            let short = format!("couldn't read file {}: {}", path2, e);
            if path2 == "A" {
                CliError {
                    short,
                    hints: Some(vec!["If you're using --no-legacy-cli, simply remove the `A` and try again".to_string()])
                }
            } else {
                short_error(short)
            }
        })
}

// I wish we could make anonymous sum types
enum SecondInputKind {
    Game(String),
    File(String),
}

fn main() {
    let cli = Cli::parse();

    let check_legacy_args = !cli.no_legacy_cli;

    let mut inputs_iter = cli.inputs.into_iter();

    let first_input = inputs_iter.next().and_then(|inp| {
        match (check_legacy_args, inp.as_str()) {
            (true, "A") => None,
            (true, "D") => display_short_error_and_exit(
                "CamCore does not support disassembly",
            ),
            (true, "AA") => display_short_error_and_exit(
                "assembly and linker script generation isn't supported yet",
            ),
            _ => Some(load_file(inp)),
        }
    });

    // if there was no first input, it doesn't matter anyway, so this is
    // simpler than, e.g., minting [FirstInputKind].
    let first_input_was_legacy = matches!(first_input, None);

    let second_input = inputs_iter.next().map(|inp| {
        if first_input_was_legacy && KNOWN_FE_GAMES.contains(&inp.as_str()) {
            SecondInputKind::Game(inp)
        } else {
            SecondInputKind::File(inp)
        }
    });

    let (second_input, game_input) = match second_input {
        None => (None, None),
        Some(SecondInputKind::Game(g)) => (None, Some(g)),
        Some(SecondInputKind::File(inp)) => (Some(load_file(inp)), None),
    };

    let inputs = vec![first_input, second_input]
        .into_iter()
        .filter_map(id)
        .chain(inputs_iter.map(|inp| load_file(inp)))
        .collect::<Result<Vec<_>, _>>();

    let inputs = match inputs {
        Ok(inputs) => inputs,
        Err(e) => display_cli_error_and_exit(e),
    };

    if inputs.is_empty() {
        display_short_error_and_exit("no input files")
    }

    let game_input = match (cli.game, game_input) {
        (None, None) => {
            display_short_error_and_exit("--game parameter is required")
        }
        (Some(game), None) | (None, Some(game)) => game,
        (Some(cli_game), Some(legacy_game)) => {
            if cli_game == legacy_game {
                cli_game
            } else {
                display_cli_error_and_exit(CliError {
                    short: "multiple games specified"
                        .to_string(),
                    hints: Some(vec![
                        format!("If you mean to assemble the file {}, run again with --no-legacy-cli.", legacy_game)
                    ]),
                });
            }
        }
    };

    let defines: Result<HashMap<_, _>, _> = cli
        .defines
        .into_iter()
        .map(|s| Ok(((), ())))
        .collect::<Result<HashMap<_, _>, _>>();
}
