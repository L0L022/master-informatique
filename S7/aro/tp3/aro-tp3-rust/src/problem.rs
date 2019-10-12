use failure::{err_msg, Fallible};

use crate::glpk;
use crate::patterns::Patterns;
use std::ptr;

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

impl Problem {
    pub fn from_str(data: &str) -> Fallible<Problem> {
        let mut lines = data.lines();

        if lines.next().ok_or(err_msg("Does not find initials"))? != "initials" {
            return bail!("Does not begin with initials");
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

    fn _solve(&self, patterns: &Patterns) -> Vec<f64> {
        unsafe {
            use glpk::*;

            let lp = glp_create_prob();

            glp_set_obj_dir(lp, GLP_MIN);

            /* fill problem */
            let rows = self.finals.len() as i32;
            glp_add_rows(lp, rows);
            for row in 0..rows {
                glp_set_row_bnds(lp, row + 1, GLP_LO, self.finals[row as usize].count as f64, 0.0);
            }

            let columns = patterns.nb_columns() as i32;
            glp_add_cols(lp, columns);
            for column in 1..=columns {
                glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
                glp_set_obj_coef(lp, column, 1.0);
            }

            patterns.glp_load_matrix(lp);

            /* solve problem */
            glp_simplex(lp, ptr::null());

            /* recover and display results */
            dbg!(glp_get_obj_val(lp));

            // result.primals.resize(columns);
            // for (int column = 1; column <= columns; ++column) {
            //     result.primals[column - 1] = glp_get_col_prim(lp, column);
            // }

            // let columns = columns as usize;
            // let mut sum = 0.0;
            // for column in 1..=columns {
            //     sum += glp_get_col_prim(lp, column as i32);
            // }

            let rows = rows as usize;
            let mut duals = vec![0.0; rows];
            for row in 1..=rows {
                duals[row - 1] = glp_get_row_dual(lp, row as i32);
            }

            /* housekeeping */
            glp_delete_prob(lp);
            glp_free_env();

            duals
        }
    }

    fn _solve1(&self, initial_size: u32, duals: Vec<f64>) -> (f64, Vec<f64>) {
        unsafe {
            use glpk::*;

            let lp = glp_create_prob();

            glp_set_obj_dir(lp, GLP_MAX);

            /* fill problem */
            glp_add_rows(lp, 1);
            glp_set_row_bnds(lp, 1, GLP_UP, 0.0, initial_size as f64);

            let columns = duals.len() as i32;
            glp_add_cols(lp, columns);
            for column in 1..=columns {
                glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
                glp_set_obj_coef(lp, column, duals[column as usize - 1]);
            }

            let mut patterns = Patterns::new(1);
            for column in 1..=columns {
                patterns.push_signle_line(1, self.finals[column as usize - 1].width as f64);
            }

            patterns.glp_load_matrix(lp);

            /* solve problem */
            glp_simplex(lp, ptr::null());

            /* recover and display results */
            let obj = dbg!(glp_get_obj_val(lp));

            // result.primals.resize(columns);
            // for (int column = 1; column <= columns; ++column) {
            //     result.primals[column - 1] = glp_get_col_prim(lp, column);
            // }
            
            let columns = columns as usize;
            let mut xs = vec![0.0; columns];
            for column in 1..=columns {
                xs[column - 1] = glp_get_col_prim(lp, column as i32);
            }

            dbg!(&xs);

            /* housekeeping */
            glp_delete_prob(lp);
            glp_free_env();

            (obj, xs)
        }
    }

    pub fn solve(&self) {
        let initial = &self.initials[0];
        let mut patterns = Patterns::new(self.finals.len());

        for i in 1..=self.finals.len() {
            patterns.push_signle_line(i, (initial.width / self.finals[i - 1].width) as f64)
        }

        let mut i = 0;
        loop {
            i+=1;
            //if i > 3 { break; }
            patterns.print();

            let duals = dbg!(self._solve(&patterns));
            let (obj, xs) = self._solve1(initial.width, duals);

            if obj - 0.00001 < 1.0 { break; }
            patterns.push(&xs[..]);
        }

        dbg!(i);
    }
}
