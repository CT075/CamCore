
// XXX - fork chomp to avoid needing this
#[macro_use]
extern crate chomp;

#[macro_use]
extern crate failure_derive;

mod camlib;

mod parse;
mod raws;
mod lang;

fn main() {
    println!("Hello, world!");
}
