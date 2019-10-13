use crate::glpk;
use std::fmt;

pub struct Matrix {
  nb_rows: usize,
  nb_columns: usize,
  i: Vec<i32>,
  j: Vec<i32>,
  v: Vec<f64>,
}

impl Matrix {
  pub fn new(nb_rows: usize) -> Self {
    Matrix {
      nb_rows,
      nb_columns: 0,
      i: vec![0; 1],
      j: vec![0; 1],
      v: vec![0.0; 1],
    }
  }

  pub fn push(&mut self, rows: &[f64]) {
    self.nb_columns += 1;

    for (i, &row) in rows.iter().enumerate().take(self.nb_rows) {
      if row == 0.0 {
        continue;
      }
      self.i.push(i as i32 + 1);
      self.j.push(self.nb_columns as i32);
      self.v.push(row);
    }
  }

  pub fn push_signle_row(&mut self, i: usize, value: f64) {
    self.nb_columns += 1;
    self.i.push(i as i32);
    self.j.push(self.nb_columns as i32);
    self.v.push(value);
  }

  pub fn nb_rows(&self) -> usize {
    self.nb_rows
  }

  pub fn nb_columns(&self) -> usize {
    self.nb_columns
  }

  pub fn size(&self) -> usize {
    self.nb_rows * self.nb_columns
  }

  pub fn get(&self, i: usize, j: usize) -> f64 {
    for k in 1..self.v.len() {
      if self.i[k] != i as i32 || self.j[k] != j as i32 {
        continue;
      }
      return self.v[k];
    }

    0.0
  }

  pub unsafe fn glp_load_matrix(&self, lp: *mut glpk::glp_prob) {
    glpk::glp_load_matrix(
      lp,
      self.i.len() as i32 - 1,
      self.i.as_ptr(),
      self.j.as_ptr(),
      self.v.as_ptr(),
    );
  }
}

struct Row<'a> {
  matrix: &'a Matrix,
  i: usize,
}

impl<'a> Row<'a> {
  fn new(matrix: &'a Matrix, row: usize) -> Self {
    Self { matrix, i: row }
  }
}

impl<'a> fmt::Debug for Row<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list()
      .entries(RowIt::new(self.matrix, self.i))
      .finish()
  }
}

struct RowIt<'a> {
  matrix: &'a Matrix,
  i: usize,
  j: usize,
}

impl<'a> RowIt<'a> {
  fn new(matrix: &'a Matrix, row: usize) -> Self {
    Self {
      matrix,
      i: row,
      j: 1,
    }
  }
}

impl<'a> Iterator for RowIt<'a> {
  type Item = f64;

  fn next(&mut self) -> Option<f64> {
    if self.j > self.matrix.nb_columns {
      return None;
    }

    let v = self.matrix.get(self.i, self.j);
    self.j += 1;

    Some(v)
  }
}

impl fmt::Debug for Matrix {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if f.alternate() {
      writeln!(f, "[")?;
      for i in 1..=self.nb_rows() {
        writeln!(f, "    {:?},", &Row::new(&self, i))?;
      }
      write!(f, "]")
    } else {
      write!(f, "[")?;
      for i in 1..=self.nb_rows {
        if i > 1 {
          write!(f, ", ")?;
        }
        write!(f, "{:?}", &Row::new(&self, i))?;
      }
      write!(f, "]")
    }
  }
}
