extern crate bindgen;

use std::env;
use std::path::PathBuf;
use bindgen::callbacks::ParseCallbacks;
use bindgen::callbacks::IntKind;

#[derive(Debug)]
struct MyParseCallbacks;

impl ParseCallbacks for MyParseCallbacks {
    fn int_macro(&self, _name: &str, _value: i64) -> Option<IntKind> {
        Some(IntKind::I32)
    }
}

fn main() {
    // Tell cargo to tell rustc to link the system glpk
    // shared library.
    println!("cargo:rustc-link-lib=glpk");

    // Tell cargo to invalidate the built crate whenever the wrapper changes
    println!("cargo:rerun-if-changed=wrapper.h");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::builder()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        .parse_callbacks(Box::new(MyParseCallbacks))
        .rustfmt_bindings(true)
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
