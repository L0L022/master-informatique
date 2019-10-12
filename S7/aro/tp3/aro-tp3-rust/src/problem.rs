use failure::{err_msg, Fallible};

use crate::glpk;
use crate::patterns::Patterns;
use std::ptr;
use std::ffi::CStr;
use std::io::Write;
use std::pin::Pin;

#[derive(Debug)]
struct Initial {
    width: u32,
    something: f64,
}

#[derive(Debug)]
struct Final {
    count: u32,
    width: u32,
}

#[derive(Debug)]
pub struct Problem {
    initials: Vec<Initial>,
    finals: Vec<Final>,
}

#[derive(Debug, Default)]
struct SolveData {
    objective: f64,
    primals: Vec<f64>,
    duals: Vec<f64>,
    pattern: Vec<f64>,
}

struct TerminalOutput(*mut dyn Write);

unsafe extern "C" fn terminal_output(info: *mut libc::c_void, s: *const libc::c_char) -> libc::c_int {
    let output = &mut *(*(info as *mut TerminalOutput)).0;
    output.write_all(CStr::from_ptr(s).to_bytes());
    // output.push_str(&CStr::from_ptr(s).to_string_lossy().into_owned());
    1
}

impl Problem {
    pub fn from_str(data: &str) -> Fallible<Problem> {
        let mut lines = data.lines();

        if lines.next().ok_or(err_msg("Does not find initials"))? != "initials" {
            bail!("Does not begin with initials");
        }

        let mut initials = Vec::new();
        while let Some(line) = lines.next() {
            if line == "finals" { break; }
            let (width, something) = scan_fmt!(line, "{} {}", u32, f64)?;
            initials.push(Initial { width, something });
        }

        let mut finals = Vec::new();
        while let Some(line) = lines.next() {
            let (width, count) = scan_fmt!(line, "{} {}", u32, u32)?;
            finals.push(Final { count, width });
        }

        Ok(Problem { initials, finals })
    }

    fn _solve(&self, patterns: &Patterns, data: &mut SolveData) -> Fallible<()> {
        unsafe {
            use glpk::*;
            glp_init_env();

            let mut output = Vec::new();
            let mut to = TerminalOutput(&mut output);
            glp_term_hook(Some(terminal_output), &mut to as *mut _ as *mut libc::c_void);
            
            let lp = glp_create_prob();
            glp_set_obj_dir(lp, GLP_MIN);

            let rows = self.finals.len() as i32;
            glp_add_rows(lp, rows);
            for row in 0..rows {
                let b =  self.finals[row as usize].count as f64;
                glp_set_row_bnds(lp, row + 1, GLP_FX, b, b);
            }

            let columns = patterns.nb_columns() as i32;
            glp_add_cols(lp, columns);
            for column in 1..=columns {
                glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
                glp_set_obj_coef(lp, column, 1.0);
            }

            patterns.glp_load_matrix(lp);

            ensure!(glp_simplex(lp, ptr::null()) == 0, String::from_utf8_lossy(&output).into_owned());

            data.objective = glp_get_obj_val(lp);

            let columns = columns as usize;
            data.primals.resize(columns, 0.0);
            for column in 1..=columns {
                data.primals[column - 1] = glp_get_col_prim(lp, column as i32);
            }

            let rows = rows as usize;
            data.duals.resize(rows, 0.0);
            for row in 1..=rows {
                data.duals[row - 1] = glp_get_row_dual(lp, row as i32);
            }

            glp_delete_prob(lp);
            glp_free_env();

            // std::io::stderr().write_all(&output)?;

            Ok(())
        }
    }

    fn _solve1(&self, initial_size: u32, data: &mut SolveData) -> Fallible<f64> {
        unsafe {
            use glpk::*;
            glp_init_env();

            let mut output = Vec::new();
            let mut to = TerminalOutput(&mut output);
            glp_term_hook(Some(terminal_output), &mut to as *mut _ as *mut libc::c_void);

            let lp = glp_create_prob();
            glp_set_obj_dir(lp, GLP_MAX);

            glp_add_rows(lp, 1);
            glp_set_row_bnds(lp, 1, GLP_UP, 0.0, initial_size as f64);

            let columns = data.duals.len() as i32;
            glp_add_cols(lp, columns);
            for column in 1..=columns {
                glp_set_col_kind(lp, column, GLP_IV);
                glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
                glp_set_obj_coef(lp, column, data.duals[column as usize - 1]);
            }

            let mut patterns = Patterns::new(1);
            for column in 1..=columns {
                patterns.push_signle_line(1, self.finals[column as usize - 1].width as f64);
            }

            patterns.glp_load_matrix(lp);
            
            ensure!(glp_simplex(lp, ptr::null()) == 0, String::from_utf8_lossy(&output).into_owned());
            ensure!(glp_intopt(lp, ptr::null()) == 0, String::from_utf8_lossy(&output).into_owned());

            let obj = dbg!(glp_mip_obj_val(lp));
            
            let columns = columns as usize;
            data.pattern.resize(columns, 0.0);
            for column in 1..=columns {
                data.pattern[column - 1] = glp_mip_col_val(lp, column as i32);
            }

            glp_delete_prob(lp);
            glp_free_env();

            Ok(obj)
        }
    }

    pub fn solve(&self) -> Fallible<(f64, Vec<f64>)> {
        let initial = &self.initials[0];
        let mut patterns = Patterns::new(self.finals.len());

        for i in 1..=self.finals.len() {
            patterns.push_signle_line(i, (initial.width / self.finals[i - 1].width) as f64)
        }

        let mut data = SolveData::default();

        let mut i = 0;
        loop {
            i+=1;
            //if i > 3 { break; }
            //patterns.print();

            self._solve(&patterns, &mut data)?;
            let obj = self._solve1(initial.width, &mut data)?;

            if obj - 0.00001 < 1.0 { break; }
            patterns.push(&data.pattern[..]);
        }

        dbg!(i);
        Ok((data.objective, data.primals))
    }
}
