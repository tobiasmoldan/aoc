use std::{fs, path::PathBuf};

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
struct World {
    inner: Vec<Vec<char>>,
}

impl World {
    fn positions(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        self.inner
            .iter()
            .enumerate()
            .flat_map(|(y, e)| e.iter().enumerate().map(move |(x, _)| (x, y)))
    }

    fn get(&self, x: i32, y: i32) -> Option<char> {
        let Ok(x) = usize::try_from(x) else {
            return None;
        };
        let Ok(y) = usize::try_from(y) else {
            return None;
        };
        self.inner.get(y).and_then(|e| e.get(x)).copied()
    }
}

fn part_1(world: World) -> Result<i32> {
    let scans: [[(i32, i32, _); 4]; 8] = [
        [(0, 0, 'X'), (1, 0, 'M'), (2, 0, 'A'), (3, 0, 'S')],
        [(0, 0, 'X'), (0, 1, 'M'), (0, 2, 'A'), (0, 3, 'S')],
        [(0, 0, 'X'), (-1, 0, 'M'), (-2, 0, 'A'), (-3, 0, 'S')],
        [(0, 0, 'X'), (0, -1, 'M'), (0, -2, 'A'), (0, -3, 'S')],
        [(0, 0, 'X'), (1, 1, 'M'), (2, 2, 'A'), (3, 3, 'S')],
        [(0, 0, 'X'), (1, -1, 'M'), (2, -2, 'A'), (3, -3, 'S')],
        [(0, 0, 'X'), (-1, 1, 'M'), (-2, 2, 'A'), (-3, 3, 'S')],
        [(0, 0, 'X'), (-1, -1, 'M'), (-2, -2, 'A'), (-3, -3, 'S')],
    ];

    Ok(world
        .positions()
        .map(|(x, y)| {
            scans
                .iter()
                .filter(|matches| {
                    for (dx, dy, expect) in matches.iter() {
                        if world.get(x as i32 + dx, y as i32 + dy) != Some(*expect) {
                            return false;
                        }
                    }
                    true
                })
                .count() as i32
        })
        .sum())
}

fn part_2(world: World) -> Result<i32> {
    let scans: [[(i32, i32, _); 5]; 4] = [
        [
            (0, 0, 'A'),
            (-1, -1, 'M'),
            (1, 1, 'S'),
            (1, -1, 'M'),
            (-1, 1, 'S'),
        ],
        [
            (0, 0, 'A'),
            (-1, -1, 'M'),
            (1, 1, 'S'),
            (1, -1, 'S'),
            (-1, 1, 'M'),
        ],
        [
            (0, 0, 'A'),
            (-1, -1, 'S'),
            (1, 1, 'M'),
            (1, -1, 'M'),
            (-1, 1, 'S'),
        ],
        [
            (0, 0, 'A'),
            (-1, -1, 'S'),
            (1, 1, 'M'),
            (1, -1, 'S'),
            (-1, 1, 'M'),
        ],
    ];

    Ok(world
        .positions()
        .map(|(x, y)| {
            scans
                .iter()
                .filter(|matches| {
                    for (dx, dy, expect) in matches.iter() {
                        if world.get(x as i32 + dx, y as i32 + dy) != Some(*expect) {
                            return false;
                        }
                    }
                    true
                })
                .count() as i32
        })
        .sum())
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let world = World {
        inner: content.lines().map(|l| l.chars().collect()).collect(),
    };

    let result = match part {
        1 => part_1(world),
        2 => part_2(world),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
