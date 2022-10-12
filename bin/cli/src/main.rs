use clap::Parser;

#[derive(Parser)]
#[command(name = "CamCore")]
#[command(author = "CT075 <cam@camdar.io>")]
#[command(version)]
#[command(about = "An Event Assembler", long_about = None)]
struct Opts {
    game: String,
    #[arg(long)]
    debug: bool,
    #[arg(short, long)]
    verbose: bool,
    #[arg(short, long)]
    quiet: bool,
    #[arg(long)]
    build_times: bool,
    #[arg(long)]
    nocash_sym: bool,
    #[arg(long)]
    color: bool,
}

fn main() {
    println!("Hello, world!");
}
