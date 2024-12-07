use std::{
    collections::{BTreeSet, HashSet},
    fmt::Write,
    fs,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use clap::Parser as _;
use eyre::{eyre, OptionExt, Result};
use itertools::Itertools;
use rayon::prelude::*;

#[derive(clap::Parser)]
#[command()]
struct Args {
    #[arg()]
    part: usize,
    #[arg()]
    file: PathBuf,
}

#[derive(Debug, Clone, Copy)]
enum WorldTile {
    Obstructed,
    Empty,
    Outside,
}

#[derive(Clone)]
struct World {
    width: usize,
    height: usize,
    tiles: Arc<[WorldTile]>,
    initial_guard_position: (usize, usize),
    optional_obstruction: Option<(usize, usize)>,
}

impl std::fmt::Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.tiles.chunks(self.width) {
            for tile in line {
                match tile {
                    WorldTile::Obstructed => f.write_char('#'),
                    WorldTile::Empty => f.write_char('.'),
                    WorldTile::Outside => unreachable!(),
                }?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl World {
    fn guard(&self) -> Guard<'_> {
        Guard {
            direction: Direction::North,
            x: self.initial_guard_position.0 as i32,
            y: self.initial_guard_position.1 as i32,
            done: false,
            started: false,
            world: self,
        }
    }

    fn tile(&self, x: i32, y: i32) -> WorldTile {
        let Ok(x) = usize::try_from(x) else {
            return WorldTile::Outside;
        };
        let Ok(y) = usize::try_from(y) else {
            return WorldTile::Outside;
        };
        if x >= self.width {
            return WorldTile::Outside;
        };
        if y >= self.height {
            return WorldTile::Outside;
        };

        if let Some((blocked_x, blocked_y)) = self.optional_obstruction {
            if blocked_x == x && blocked_y == y {
                return WorldTile::Obstructed;
            }
        }

        *self.tiles.get(y * self.height + x).unwrap()
    }

    fn with_obstruction(&self, x: i32, y: i32) -> Result<Self> {
        let Ok(x) = usize::try_from(x) else {
            return Err(eyre!("x = {x} is not usize"));
        };
        let Ok(y) = usize::try_from(y) else {
            return Err(eyre!("y = {y} is not usize"));
        };

        Ok(Self {
            height: self.height,
            width: self.width,
            tiles: Arc::clone(&self.tiles),
            initial_guard_position: self.initial_guard_position,
            optional_obstruction: Some((x, y)),
        })
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Direction {
    West,
    North,
    East,
    South,
}

impl Direction {
    fn next(&self) -> Self {
        match self {
            Direction::West => Direction::North,
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
        }
    }
}

struct Guard<'a> {
    world: &'a World,
    x: i32,
    y: i32,
    direction: Direction,
    done: bool,
    started: bool,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct GuardState {
    x: i32,
    y: i32,
    direction: Direction,
}

fn in_front(x: i32, y: i32, dir: Direction) -> (i32, i32) {
    let (dx, dy) = match dir {
        Direction::West => (-1, 0),
        Direction::North => (0, -1),
        Direction::East => (1, 0),
        Direction::South => (0, 1),
    };

    (x + dx, y + dy)
}

impl Iterator for Guard<'_> {
    type Item = GuardState;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.started {
            self.started = true;
            return Some(GuardState {
                x: self.x,
                y: self.y,
                direction: self.direction,
            });
        }

        if self.done {
            return None;
        }

        let (x, y) = in_front(self.x, self.y, self.direction);

        match self.world.tile(x, y) {
            WorldTile::Obstructed => self.direction = self.direction.next(),
            WorldTile::Empty => {
                self.x = x;
                self.y = y;
            }
            WorldTile::Outside => {
                self.done = true;
                return None;
            }
        }

        Some(GuardState {
            x: self.x,
            y: self.y,
            direction: self.direction,
        })
    }
}

impl std::str::FromStr for World {
    type Err = eyre::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (width, height, guard_pos, tiles): (usize, usize, Option<(usize, usize)>, Vec<_>) =
            s.chars().enumerate().try_fold(
                (0, 1, None, Vec::new()),
                |(mut width, mut height, mut guard, mut vec), (i, e)| {
                    match e {
                        '^' => {
                            let _ = std::mem::replace(
                                &mut guard,
                                Some((
                                    if height == 1 { width } else { i % (width + 1) },
                                    height - 1,
                                )),
                            );

                            if height == 1 {
                                width += 1;
                            }
                            vec.push(WorldTile::Empty);
                        }
                        '.' => {
                            if height == 1 {
                                width += 1;
                            }
                            vec.push(WorldTile::Empty);
                        }
                        '#' => {
                            if height == 1 {
                                width += 1;
                            }
                            vec.push(WorldTile::Obstructed);
                        }
                        '\n' => height += 1,
                        _ => return Err(eyre!("unknown tile: {e}")),
                    }
                    Ok::<_, Self::Err>((width, height, guard, vec))
                },
            )?;

        Ok(World {
            width,
            height,
            tiles: Arc::from(tiles.into_boxed_slice()),
            initial_guard_position: guard_pos.ok_or_eyre("did not find guard")?,
            optional_obstruction: None,
        })
    }
}

fn part_1(world: World) -> Result<i32> {
    let guard = world.guard();

    Ok(guard.map(|s| (s.x, s.y)).unique().count() as i32)
}

fn part_2(world: World) -> Result<i32> {
    let guard = world.guard();

    let steps = guard
        .map(|state| in_front(state.x, state.y, state.direction))
        .unique()
        .collect::<Vec<_>>();
    let steps = &steps[..(steps.len() - 1)];

    Ok(steps
        .into_par_iter() // not really needed, 1.5sec improvement for --release builds
        .flat_map(|next| {
            let world = world.with_obstruction(next.0, next.1).unwrap();

            let mut past_states = HashSet::new();
            let simulated_guard = world.guard();

            for state in simulated_guard {
                if past_states.contains(&state) {
                    return Some(next);
                } else {
                    past_states.insert(state);
                }
            }

            None
        })
        .count() as i32)
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;
    let world = World::from_str(content.trim())?;

    println!("{} {}", world.width, world.height);

    let result = match part {
        1 => part_1(world),
        2 => part_2(world),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
