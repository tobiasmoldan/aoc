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

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let height = content.trim().lines().count();
    let width = content.trim().lines().next().unwrap().chars().count();

    let antennas = content.trim().lines().enumerate().flat_map(|(y, line)| {
        line.chars().enumerate().flat_map(move |(x, char)| {
            if char.is_alphanumeric() {
                Some((char, x, y))
            } else {
                None
            }
        })
    });

    let result = match part {
        1 => part1::run(width, height, antennas),
        2 => part2::run(width, height, antennas),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
