use std::{
    fs, path::PathBuf,
};

use clap::Parser as _;
use eyre::{eyre, Result};

#[derive(clap::Parser)]
#[command()]
struct Args {
    #[arg()]
    part: usize,
    #[arg()]
    file: PathBuf,
}

fn test_levels(levels: &[i32], skip: Option<usize>) -> std::result::Result<(), usize> {
    fn test(inc: bool, prev: i32, current: i32) -> bool {
        (
            (inc && prev < current) ||  // test if inc prev smaller than current
            (!inc && prev > current)    // test if not inc prev greater than current
        ) && 
        (prev - current).abs() >= 1 &&  // test diff >= 1
        (prev - current).abs() <= 3     // test diff <= 3
    }

    levels
        .iter()
        .enumerate()
        .filter(|(i, _)| Some(*i) != skip)
        .try_fold((None, None), |(prev, inc), (pos, level)| {
            match (prev, inc) {
                (None, _) => Ok((Some(*level), None)),
                (Some(prev), None) => {
                    let inc = prev < *level;
                    if test(inc, prev, *level) {
                        Ok((Some(*level), Some(inc)))
                    } else {
                        Err(pos)
                    }
                }
                (Some(prev), Some(inc)) => if test(inc, prev, *level) {
                    Ok((Some(*level), Some(inc)))
                } else {
                    Err(pos)
                }
            }
        })
        .map(|_| ())
}

fn part_1(records: Vec<Vec<i32>>) -> Result<usize> {
    let x = records
        .into_iter()
        .flat_map(|levels| test_levels(&levels, None).ok())
        .count();

    Ok(x)
}

fn part_2(records: Vec<Vec<i32>>) -> Result<usize> {
    let x = records
        .into_iter()
        .filter(|levels| {
            // brute force for the win
            test_levels(levels, None).is_ok() || (0..levels.len()).any(|x| test_levels(levels, Some(x)).is_ok())
        })
        .count();
    Ok(x)
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let records = content
        .lines()
        .map(|l| {
            l.split_whitespace()
                .filter(|s| !s.is_empty())
                .map(|s| s.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    let result = match part {
        1 => part_1(records),
        2 => part_2(records),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
