use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::PathBuf,
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

#[derive(Debug)]
struct Input {
    ordering_rules: Vec<(usize, usize)>,
    updates: Vec<Vec<usize>>,
}

fn filter_rules<'a>(
    pages: &'a [usize],
    rules: &'a BTreeSet<(usize, usize)>,
) -> impl Iterator<Item = (usize, usize)> + 'a {
    itertools::iproduct!(pages.iter(), pages.iter())
        .map(|(x, y)| (*x, *y))
        .filter(|(x, y)| x != y)
        .filter(|x| rules.contains(x))
}

fn part_1(input: Input) -> Result<i32> {
    let rules = BTreeSet::from_iter(input.ordering_rules);

    Ok(input
        .updates
        .into_iter()
        .filter(|pages| {
            let prequesits = filter_rules(pages.as_slice(), &rules).fold(
                BTreeMap::<usize, Vec<usize>>::new(),
                |mut m, (x, y)| {
                    match m.entry(y) {
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            entry.insert(vec![x]);
                        }
                        std::collections::btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().push(x);
                        }
                    }
                    m
                },
            );

            let mut previous = BTreeSet::new();

            for page in pages {
                if prequesits
                    .get(page)
                    .into_iter()
                    .flatten()
                    .all(|p| previous.contains(p))
                {
                    previous.insert(*page);
                } else {
                    return false;
                }
            }

            true
        })
        .flat_map(|pages| pages.get((pages.len() - 1) / 2).copied())
        .sum::<usize>() as i32)
}

fn part_2(input: Input) -> Result<i32> {
    let rules = BTreeSet::from_iter(input.ordering_rules);
    Ok(input
        .updates
        .into_iter()
        .flat_map(|pages| {
            let prequesits = filter_rules(pages.as_slice(), &rules).fold(
                BTreeMap::<usize, Vec<usize>>::new(),
                |mut m, (x, y)| {
                    match m.entry(y) {
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            entry.insert(vec![x]);
                        }
                        std::collections::btree_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().push(x);
                        }
                    }
                    m
                },
            );

            let mut previous = BTreeSet::new();

            let mut is_err = false;
            for page in &pages {
                if prequesits
                    .get(page)
                    .into_iter()
                    .flatten()
                    .all(|p| previous.contains(p))
                {
                    previous.insert(*page);
                } else {
                    is_err = true;
                    break;
                }
            }

            if !is_err {
                return None;
            }

            let mut pages = pages;

            'm: loop {
                for i in 0..pages.len() {
                    if let Some(prequisites) = prequesits.get(&pages[i]) {
                        for p in prequisites {
                            if !&pages[..i].contains(p) {
                                if let Some(j) = &pages[i..]
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, e)| if e == p { Some(i) } else { None })
                                    .next()
                                {
                                    pages.swap(i, i + j);

                                    continue 'm;
                                }
                            }
                        }
                    }
                }
                break;
            }

            Some(pages)
        })
        .flat_map(|pages| pages.get((pages.len() - 1) / 2).copied())
        .sum::<usize>() as i32)
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let mut lines = content.lines().peekable();

    let mut ordering_rules: Vec<(usize, usize)> = Vec::new();

    while lines.peek().map(|l| !l.trim().is_empty()).unwrap_or(false) {
        let (num1, num2) = lines.next().unwrap().split_once('|').unwrap();
        ordering_rules.push((num1.parse().unwrap(), num2.parse().unwrap()));
    }

    lines.next();

    let mut updates = Vec::new();

    for line in lines {
        updates.push(line.split(',').map(|num| num.parse().unwrap()).collect());
    }

    let input = Input {
        ordering_rules,
        updates,
    };

    let result = match part {
        1 => part_1(input),
        2 => part_2(input),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
