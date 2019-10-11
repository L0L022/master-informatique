use failure::{err_msg, Fallible};

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
}
