use std::env;
use std::fs;

fn main() {
    let contents = read_file();

    let input: Vec<(&str, &str)> = contents
        .split(",")
        .map(|x| x.split_once("-").unwrap())
        .collect();

    let res1: u64 = input
        .iter()
        .clone()
        .flat_map(|(x, y)| {
            let start: u64 = x.trim_end().parse().unwrap();
            let end = y.trim_end().parse().unwrap();
            (start..end).filter(|n| {
                let s = n.to_string();
                let (a, b) = s.split_at(s.len() / 2);
                a == b
            })
        })
        .sum();

    println!("Total sum part 1: {}", res1);

    let res2: u64 = input
        .into_iter()
        .flat_map(|(x, y)| {
            let start: u64 = x.trim_end().parse().unwrap();
            let end = y.trim_end().parse().unwrap();
            (start..end).filter(|n| {
                let ns = n.to_string();
                let l = ns.len();
                (1..=l / 2).filter(move |v| l % v == 0).any(move |v| {
                    ns.chars()
                        .collect::<Vec<char>>()
                        .chunks(v)
                        .collect::<Vec<&[char]>>()
                        .windows(2)
                        .all(|w| w[0] == w[1])
                })
            })
        })
        .sum();

    println!("Total sum part 2: {}", res2);
}

fn read_file() -> String {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
    }
    fs::read_to_string(&args[1]).expect("Failed to read input file")
}
