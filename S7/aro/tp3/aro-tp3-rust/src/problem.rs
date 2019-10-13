use failure::{err_msg, Fallible};

use crate::glpk;
use crate::matrix::Matrix;
use failure::Error;
use std::{ptr, str::FromStr};

#[derive(Debug, Clone, Copy)]
pub struct Initial {
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

#[derive(Debug)]
pub struct Solution {
  initial: Initial,
  iterations: usize,
  objective: f64,
  primals: Vec<f64>,
  matrix: Matrix,
}

impl Problem {
  pub fn solve(&self) -> Option<Solution> {
    self
      .initials
      .iter()
      .filter_map(|i| self.solve_initial(i).ok())
      .min_by(|l, r| l.objective.partial_cmp(&r.objective).unwrap())
  }

  fn solve_initial(&self, initial: &Initial) -> Fallible<Solution> {
    let mut matrix = Matrix::new(self.finals.len());

    for i in 1..=self.finals.len() {
      matrix.push_signle_row(i, f64::from(initial.width / self.finals[i - 1].width))
    }

    let mut data = SolveData::default();

    let mut iterations = 0;
    loop {
      self.solve_cut(&matrix, &mut data)?;
      let obj = self.solve_knapsack(initial.width, &mut data)?;

      if obj - 0.000000001 < 1.0 {
        break;
      }

      matrix.push(&data.pattern[..]);
      iterations += 1;
    }

    dbg!(iterations);
    Ok(Solution {
      initial: *initial,
      iterations,
      objective: data.objective,
      primals: data.primals,
      matrix,
    })
  }

  fn solve_cut(&self, matrix: &Matrix, data: &mut SolveData) -> Fallible<()> {
    unsafe {
      use glpk::*;
      glp_init_env();

      let to = TerminalOutput::new_and_hook();

      let lp = glp_create_prob();
      glp_set_obj_dir(lp, GLP_MIN);

      let rows = self.finals.len() as i32;
      glp_add_rows(lp, rows);
      for row in 0..rows {
        let b = f64::from(self.finals[row as usize].count);
        glp_set_row_bnds(lp, row + 1, GLP_FX, b, b);
      }

      let columns = matrix.nb_columns() as i32;
      glp_add_cols(lp, columns);
      for column in 1..=columns {
        glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
        glp_set_obj_coef(lp, column, 1.0);
      }

      matrix.glp_load_matrix(lp);

      ensure!(glp_simplex(lp, ptr::null()) == 0, to.output().to_owned());

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

      Ok(())
    }
  }

  fn solve_knapsack(&self, initial_size: u32, data: &mut SolveData) -> Fallible<f64> {
    unsafe {
      use glpk::*;
      glp_init_env();

      let to = TerminalOutput::new_and_hook();

      let lp = glp_create_prob();
      glp_set_obj_dir(lp, GLP_MAX);

      glp_add_rows(lp, 1);
      glp_set_row_bnds(lp, 1, GLP_UP, 0.0, f64::from(initial_size));

      let columns = data.duals.len() as i32;
      glp_add_cols(lp, columns);
      for column in 1..=columns {
        glp_set_col_kind(lp, column, GLP_IV);
        glp_set_col_bnds(lp, column, GLP_LO, 0.0, 0.0);
        glp_set_obj_coef(lp, column, data.duals[column as usize - 1]);
      }

      let mut matrix = Matrix::new(1);
      for column in 1..=columns {
        matrix.push_signle_row(1, f64::from(self.finals[column as usize - 1].width));
      }

      matrix.glp_load_matrix(lp);

      ensure!(glp_simplex(lp, ptr::null()) == 0, to.output().to_owned());
      ensure!(glp_intopt(lp, ptr::null()) == 0, to.output().to_owned());

      let obj = glp_mip_obj_val(lp);

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
}

impl FromStr for Problem {
  type Err = Error;

  fn from_str(data: &str) -> Fallible<Problem> {
    let mut lines = data.lines();

    if lines
      .next()
      .ok_or_else(|| err_msg("Does not find initials"))?
      != "initials"
    {
      bail!("Does not begin with initials");
    }

    let mut initials = Vec::new();
    while let Some(line) = lines.next() {
      if line == "finals" {
        break;
      }
      let (width, something) = scan_fmt!(line, "{} {}", u32, f64)?;
      initials.push(Initial { width, something });
    }

    let mut finals = Vec::new();
    for line in lines {
      let (width, count) = scan_fmt!(line, "{} {}", u32, u32)?;
      finals.push(Final { count, width });
    }

    Ok(Problem { initials, finals })
  }
}
