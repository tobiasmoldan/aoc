use std::{char, fs, path::PathBuf};

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

fn parse_mul(chars: &str) -> Option<(usize, i32, i32)> {
    let mut chars = chars.chars().peekable();

    if !matches!(
        (chars.next(), chars.next(), chars.next(), chars.next()),
        (Some('m'), Some('u'), Some('l'), Some('('))
    ) {
        return None;
    }

    let mut count = 4;

    let mut num1 = String::new();

    match chars.next() {
        Some(char) if char.is_ascii_digit() => {
            num1.push(char);
            count += 1;
        }
        _ => return None,
    };

    for _ in 1..=2 {
        match chars.peek() {
            Some(char) if char.is_ascii_digit() => {
                num1.push(*char);
                chars.next();
                count += 1;
            }
            _ => break,
        }
    }

    if !matches!(chars.next(), Some(',')) {
        return None;
    }
    count += 1;

    let mut num2 = String::new();

    match chars.next() {
        Some(char) if char.is_ascii_digit() => {
            num2.push(char);
            count += 1;
        }
        _ => return None,
    };

    for _ in 1..=2 {
        match chars.peek() {
            Some(char) if char.is_ascii_digit() => {
                num2.push(*char);
                chars.next();
                count += 1;
            }
            _ => break,
        }
    }

    if !matches!(chars.next(), Some(')')) {
        return None;
    }
    count += 1;

    Some((count, num1.parse().unwrap(), num2.parse().unwrap()))
}

fn parse_do(chars: &str) -> bool {
    let mut chars = chars.chars();

    matches!(
        (chars.next(), chars.next(), chars.next(), chars.next()),
        (Some('d'), Some('o'), Some('('), Some(')'))
    )
}

fn parse_dont(chars: &str) -> bool {
    let mut chars = chars.chars();

    matches!(
        (
            chars.next(),
            chars.next(),
            chars.next(),
            chars.next(),
            chars.next(),
            chars.next(),
            chars.next()
        ),
        (
            Some('d'),
            Some('o'),
            Some('n'),
            Some('\''),
            Some('t'),
            Some('('),
            Some(')')
        )
    )
}

fn part_1(input: String) -> Result<i32> {
    let mut chars = input.as_str();
    let mut sum = 0;

    while !chars.is_empty() {
        if let Some((count, n1, n2)) = parse_mul(chars) {
            sum += n1 * n2;
            chars = &chars[count..];
        } else {
            chars = &chars[1..];
        }
    }

    Ok(sum)
}

fn part_2(input: String) -> Result<i32> {
    let mut chars = input.as_str();
    let mut sum = 0;
    let mut is_do = true;

    while !chars.is_empty() {
        if is_do {
            if let Some((count, n1, n2)) = parse_mul(chars) {
                sum += n1 * n2;
                chars = &chars[count..];
            } else if parse_dont(chars) {
                chars = &chars[7..];
                is_do = false;
            } else {
                chars = &chars[1..];
            }
        } else {
            #[allow(clippy::collapsible_else_if)]
            if parse_do(chars) {
                chars = &chars[4..];
                is_do = true;
            } else {
                chars = &chars[1..];
            }
        }
    }

    Ok(sum)
}

fn main() -> Result<()> {
    let Args { part, file } = Args::parse();
    color_eyre::install()?;

    let content = fs::read_to_string(&file)?;

    let result = match part {
        1 => part_1(content),
        2 => part_2(content),
        x => Err(eyre!("part {x} does not exist")),
    }?;

    println!("{result}");

    Ok(())
}
