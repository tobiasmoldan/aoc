use std::{fs, path::PathBuf};

use clap::Parser as _;
use eyre::{eyre, Result};

mod part1;
mod part2;

#[derive(clap::Parser)]
#[command()]
struct Args {
    #[arg()]
    part: usize,
    #[arg()]
    file: PathBuf,
}

pub struct Equation {
    pub result: u64,
    pub numbers: Vec<u64>,
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let equations = content
        .lines()
        .flat_map(|l| l.split_once(':'))
        .flat_map(|(n1, nums)| {
            n1.parse::<u64>().ok().and_then(|res| {
                nums.split_ascii_whitespace()
                    .map(|n| n.parse::<u64>())
                    .collect::<Result<Vec<_>, _>>()
                    .ok()
                    .map(|nums| Equation {
                        result: res,
                        numbers: nums,
                    })
            })
        })
        .collect::<Vec<_>>();

    assert_eq!(content.trim().lines().count(), equations.len());

    let result = match part {
        1 => part1::run(equations),
        2 => part2::run(equations),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
