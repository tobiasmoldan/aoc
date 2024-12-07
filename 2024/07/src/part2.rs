use eyre::Result;

use crate::Equation;

#[derive(Debug)]
enum Op {
    Add,
    Mul,
    Concat,
    Unused,
}

struct OpProvider {
    inner: u32,
    len: u8,
}

impl OpProvider {
    fn new(len: usize) -> Self {
        assert!(len < 16);
        OpProvider {
            inner: 0,
            len: len as u8,
        }
    }
}

impl Iterator for OpProvider {
    type Item = OpIter;

    fn next(&mut self) -> Option<Self::Item> {
        if self.inner >= 1 << (self.len * 2) {
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

#[derive(Clone)]
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

        let val = match (self.inner & 3 << (self.pos * 2)) >> (self.pos * 2) {
            0 => Op::Add,
            1 => Op::Concat,
            2 => Op::Mul,
            3 => Op::Unused,
            _ => unreachable!(),
        };

        self.pos += 1;

        Some(val)
    }
}

pub fn run(equations: Vec<Equation>) -> Result<u64> {
    Ok(equations
        .iter()
        .flat_map(|eq| {
            if OpProvider::new(eq.numbers.len() - 1)
                .flat_map(|ops| {
                    let mut o = ops.clone();
                    if o.any(|op| matches!(op, Op::Unused)) {
                        None
                    } else {
                        Some(ops)
                    }
                })
                .any(|mut ops| {
                    let mut nums = eq.numbers.iter();
                    let Some(sum) = nums.next() else { return false };
                    let mut sum = *sum;
                    loop {
                        match (ops.next(), nums.next()) {
                            (Some(op), Some(num)) => match op {
                                Op::Add => sum += *num,
                                Op::Mul => sum *= *num,
                                Op::Concat => {
                                    let mut exp = 1u64;
                                    let mut i = *num;
                                    while i > 0 {
                                        i /= 10;
                                        exp *= 10;
                                    }
                                    sum = sum * exp + *num;
                                }
                                Op::Unused => unreachable!(),
                            },
                            (None, None) => break,
                            _ => unreachable!(),
                        }
                        if sum > eq.result {
                            return false;
                        }
                    }
                    eq.result == sum
                })
            {
                Some(eq.result)
            } else {
                None
            }
        })
        .sum())
}
