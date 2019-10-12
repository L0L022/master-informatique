extern crate aro_tp3;
extern crate structopt;

use std::fs::File;
use std::path::PathBuf;
use std::io::BufReader;
use std::io::prelude::*;use failure::Fallible;
use aro_tp3::problem::Problem;

use structopt::StructOpt;

/// TP3 ARO
#[derive(StructOpt)]
struct Opt {
    /// The instance of the problem to process
    file: PathBuf,
}

fn main() -> Fallible<()> {
    let opt = Opt::from_args();
    
    let file = File::open(opt.file)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    let (obj, prims) = Problem::from_str(&contents)?.solve()?;
    print!("objective: {}\nprimals: {:?}\n", obj, prims);

    Ok(())
}
