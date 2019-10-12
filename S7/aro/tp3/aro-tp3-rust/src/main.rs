extern crate aro_tp3;
extern crate libc;

use failure::Fallible;
use aro_tp3::problem::Problem;

fn main() -> Fallible<()> {
    Problem::from_str("initials
100 1
finals
45 97
36 610
31 395
14 211")?.solve();
Ok(())
}
