use eyre::Result;

use crate::Equation;

#[derive(Debug)]
enum Op {
    Add,
    Mul,
}

struct OpProvider {
    inner: u32,
    len: u8,
}

impl OpProvider {
    fn new(len: usize) -> Self {
        assert!(len < 32);
        OpProvider {
            inner: 0,
            len: len as u8,
        }
    }
}

impl Iterator for OpProvider {
    type Item = OpIter;

    fn next(&mut self) -> Option<Self::Item> {
        if self.inner >= 1 << self.len {
            return None;
        }

        let val = OpIter {
            inner: self.inner,
            len: self.len,
            pos: 0,
        };

        self.inner += 1;

        Some(val)
    }
}

struct OpIter {
    inner: u32,
    pos: u8,
    len: u8,
}

impl Iterator for OpIter {
    type Item = Op;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.len {
            return None;
        }

        let val = if (self.inner & 1 << self.pos) > 0 {
            Op::Add
        } else {
            Op::Mul
        };

        self.pos += 1;

        Some(val)
    }
}

pub fn run(equations: Vec<Equation>) -> Result<u64> {
    Ok(equations
        .iter()
        .flat_map(|eq| {
            let mut provider = OpProvider::new(eq.numbers.len() - 1);

            if provider.any(|mut ops| {
                let mut nums = eq.numbers.iter();
                let Some(sum) = nums.next() else { return false };
                let mut sum = *sum;
                loop {
                    match (ops.next(), nums.next()) {
                        (Some(op), Some(num)) => match op {
                            Op::Add => sum += *num,
                            Op::Mul => sum *= *num,
                        },
                        (None, None) => break,
                        _ => unreachable!(),
                    }
                    if sum > eq.result {
                        return false;
                    }
                }
                eq.result == sum
            }) {
                Some(eq.result)
            } else {
                None
            }
        })
        .sum())
}
