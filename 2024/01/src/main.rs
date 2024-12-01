use std::{fs, path::PathBuf};

use clap::Parser;

#[derive(clap::Parser)]
#[command()]
struct Args {
    #[arg()]
    part: usize,

    #[arg()]
    file_name: PathBuf,
}

fn part_1(mut left: Vec<i32>, mut right: Vec<i32>) {
    left.sort();
    right.sort();

    let sum = left
        .into_iter()
        .zip(right)
        .map(|(n1, n2)| (n1 - n2).abs())
        .sum::<i32>();

    println!("{sum}");
}

fn part_2(left: Vec<i32>, right: Vec<i32>) {
    let sum: i32 = left
        .into_iter()
        .map(|n| right.iter().filter(|n2| n == **n2).count() as i32 * n)
        .sum();
    println!("{sum}")
}

fn main() {
    let Args { part, file_name } = Args::parse();

    let content = fs::read_to_string(file_name).expect("failed to read file");

    let (v1, v2) = content
        .lines()
        .map(|l| {
            let mut nums = l.split_whitespace().take(2);
            let n1 = nums
                .next()
                .expect("failed to get num1")
                .parse::<i32>()
                .expect("failed to pase num1 as i32");
            let n2 = nums
                .next()
                .expect("failed to get num2")
                .parse::<i32>()
                .expect("failed to parse num2 as i32");
            (n1, n2)
        })
        .fold((Vec::new(), Vec::new()), |mut i, (n1, n2)| {
            i.0.push(n1);
            i.1.push(n2);
            i
        });

    match part {
        1 => part_1(v1, v2),
        2 => part_2(v1, v2),
        _ => panic!("part does not exist"),
    }
}
