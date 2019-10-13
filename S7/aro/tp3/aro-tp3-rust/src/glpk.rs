#![allow(dead_code)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use libc::{c_char, c_int, c_void};
use std::{ffi::CStr, pin::Pin};

pub struct TerminalOutput(String);

impl TerminalOutput {
  pub unsafe fn new_and_hook() -> Pin<Box<Self>> {
    let mut to = Box::pin(TerminalOutput(String::new()));
    glp_term_hook(
      Some(term_hook),
      Pin::get_unchecked_mut(Pin::as_mut(&mut to)) as *mut _ as *mut libc::c_void,
    );
    to
  }

  pub fn output(&self) -> &str {
    &self.0
  }
}

unsafe extern "C" fn term_hook(info: *mut c_void, s: *const c_char) -> c_int {
  let string = &mut (*info.cast::<TerminalOutput>()).0;
  string.push_str(&CStr::from_ptr(s).to_string_lossy().into_owned());
  1
}
