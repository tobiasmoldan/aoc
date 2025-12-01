use std::{
    env::{self},
    fs,
    path::Path,
};

fn main() {
    let mut args = env::args();
    let path = args.nth(1).expect("no file give");

    println!("part 1 {}", part_1(&path));
    println!("part 2 {}", part_2(&path));
}

fn part_1(path: impl AsRef<Path>) -> i32 {
    let f = fs::read_to_string(path).expect("unable to read file");

    let mut dial = 50;

    let mut is_zero_counter = 0;

    for line in f.lines() {
        let (direction, number) = line.split_at(1);
        let number = i32::from_str_radix(number, 10).expect("unable to parse number");
        match direction {
            "R" => {
                dial += number;
            }
            "L" => {
                dial -= number;
            }
            x => panic!("unknown direction {x}"),
        }

        while dial > 99 {
            dial -= 100;
        }

        while dial < 0 {
            dial += 100
        }

        if dial == 0 {
            is_zero_counter += 1;
        }
    }

    is_zero_counter
}

fn part_2(path: impl AsRef<Path>) -> i32 {
    let f = fs::read_to_string(path).expect("unable to read file");

    let mut dial = 50;

    let mut is_zero_counter = 0;

    for line in f.lines() {
        let was_zero = dial == 0;
        let (direction, number) = line.split_at(1);
        let number = i32::from_str_radix(number, 10).expect("unable to parse number");
        #[cfg(debug_assertions)]
        print!("  {dial}");
        match direction {
            "R" => {
                #[cfg(debug_assertions)]
                print!(" + {number}");
                dial += number;
            }
            "L" => {
                #[cfg(debug_assertions)]
                print!(" - {number}");
                dial -= number;
            }
            x => panic!("unknown direction {x}"),
        }

        #[cfg(debug_assertions)]
        print!(" = {dial}");

        while dial > 99 {
            dial -= 100;
            is_zero_counter += 1;
            #[cfg(debug_assertions)]
            print!(" | fix > 99");
        }

        while dial < 0 {
            #[cfg(debug_assertions)]
            print!(" | fix < 0");
            dial += 100;
            is_zero_counter += 1;
        }

        if was_zero {
            #[cfg(debug_assertions)]
            print!(" | was_zero correction");
            is_zero_counter -= 1;
        }

        if dial == 0 {
            is_zero_counter += 1;
            #[cfg(debug_assertions)]
            print!(" | at 0");
        }

        #[cfg(debug_assertions)]
        println!("");
    }

    is_zero_counter
}
