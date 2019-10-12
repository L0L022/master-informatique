
use crate::glpk;

// lines -> rows

pub struct Patterns {
    nb_lines: usize,
    nb_columns: usize,
    i: Vec<i32>,
    j: Vec<i32>,
    v: Vec<f64>,
}

impl Patterns {
    pub fn new(nb_lines: usize) -> Self {
        Patterns {
            nb_lines,
            nb_columns: 0,
            i: vec![0; 1],
            j: vec![0; 1],
            v: vec![0.0; 1],
        }
    }

    pub fn push(&mut self, lines: &[f64]) {
        self.nb_columns += 1;

        for line in 0..self.nb_lines {
            if lines[line] == 0.0 { continue; }
            self.i.push(line as i32 + 1);
            self.j.push(self.nb_columns as i32);
            self.v.push(lines[line]);
        }
    }

    pub fn push_signle_line(&mut self, i: usize, value: f64) {
        self.nb_columns += 1;
        self.i.push(i as i32);
        self.j.push(self.nb_columns as i32);
        self.v.push(value);
    }

    pub fn nb_lines(&self) -> usize {
        self.nb_lines
    }

    pub fn nb_columns(&self) -> usize {
        self.nb_columns
    }

    pub fn nb_patterns(&self) -> usize {
        self.nb_columns
    }

    pub fn size(&self) -> usize {
        self.nb_lines * self.nb_columns
    }

    pub fn get(&self, i: usize, j: usize) -> f64 {
        for k in 1..self.v.len() {
            if self.i[k] != i as i32 || self.j[k] != j as i32 { continue; }
            return self.v[k];
        }

        0.0
    }

    pub fn print(&self) {
        for i in 1..=self.nb_lines {
            for j in 1..=self.nb_columns {
                print!("{:.2}\t", self.get(i, j));
            }
            println!();
        }
    }

    pub fn glp_load_matrix(&self, lp: *mut glpk::glp_prob) {
        unsafe { glpk::glp_load_matrix(lp, self.i.len() as i32 - 1, self.i.as_ptr(), self.j.as_ptr(), self.v.as_ptr()); }
    }
}
