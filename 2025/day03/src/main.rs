use std::char;
use utils;

fn main() {
    let contents = utils::read_file();

    println!("Part 1: {}", joltage(&contents, 2));
    println!("Part 2: {}", joltage(&contents, 12));
}

fn joltage(banks: &str, num: usize) -> u64 {
    banks
        .lines()
        .map(|line| {
            let bank: Vec<u32> = line.chars().map(|c| c.to_digit(10).unwrap()).collect();
            let l = bank.len();

            let mut res = String::new();
            let mut bank_slice = &bank[..];
            let mut end = l - num + 1;
            for _ in 0..num {
                let m = bank_slice[0..end].iter().max().unwrap();
                let p = bank_slice.iter().position(|x| x == m).unwrap();
                res.push(char::from_digit(bank_slice[p], 10).unwrap());
                bank_slice = &bank_slice[p + 1..];
                end -= p;
            }
            res.parse::<u64>().unwrap()
        })
        .sum()
}
