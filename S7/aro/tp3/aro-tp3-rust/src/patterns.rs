
use crate::glpk;

pub struct Patterns {
    data: Vec<f64>,
    nb_lines: usize,
}

impl Patterns {
    pub fn new(nb_lines: usize) -> Self {
        Patterns {
            data: Vec::new(),
            nb_lines,
        }
    }

    pub fn resize(&mut self, nb_patterns: usize) {
        self.data.resize(self.nb_lines * nb_patterns, 0.0);
    }

    pub fn len(&self) -> usize {
        self.data.len() / self.nb_lines
    }

    pub fn push(&mut self, lines: &[f64]) {
        self.resize(self.len() + 1);
        let column = self.len() - 1;
        for line in 0..self.nb_lines {
            *self.get_mut(line, column) = lines[line];
        }
    }

    pub fn nb_lines(&self) -> usize {
        self.nb_lines
    }

    pub fn get(&self, line: usize, column: usize) -> f64 {
        return self.data[column * self.nb_lines + line];
    }

    pub fn get_mut(&mut self, line: usize, column: usize) -> &mut f64 {
        return &mut self.data[column * self.nb_lines + line];
    }

    pub fn print(&self) {
        for i in 0..self.nb_lines {
            for j in 0..self.len() {
                print!("{} ", self.get(i, j));
            }
            println!();
        }
    }

    fn glp_load_matrix(&self, lp: *mut glpk::glp_prob) {
        let nm = self.nb_lines() * self.len();
        let (ia, ja) = (vec![0i32; nm+1], vec![0i32; nm+1]);

        let k = 1;
        for j in 0..self.len() {
            for i in 0..self.nb_lines() {
                ia[k] = i as i32 + 1;
                ja[k] = j as i32 + 1;
                k += 1;
            }
        }

        unsafe { glpk::glp_load_matrix(lp, nm as i32, ia.as_ptr(), ja.as_ptr(), self.data.as_ptr()-1); }
    }
}
