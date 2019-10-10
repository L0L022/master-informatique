pub mod glpk;

struct Initial {
    width: u32,
    something: f64,
}

struct Final {
    count: u32,
    width: u32,
}

struct Problem {
    initials: Vec<Initial>,
    finals: Vec<Final>,
}