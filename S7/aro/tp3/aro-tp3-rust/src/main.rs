extern crate aro_tp3;
extern crate structopt;

use aro_tp3::problem::Problem;
use failure::Fallible;
use std::{
  fs::File,
  io::{prelude::*, BufReader},
  path::PathBuf,
  str::FromStr,
};

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

  let sol = Problem::from_str(&contents)?.solve();
  println!("{:#?}", sol);

  Ok(())
}
