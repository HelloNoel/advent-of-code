use std::env;
use std::fs;

fn main() {
    let contents = read_file();

    let input = contents.split(",").map(|x| x.split_once("-").unwrap());

    // let p: Vec<(&str, &str)> = input.collect();
    // println!("{:?}", p);

    let res: i64 = input
        .map(|(x, y)| {
            let start: i64 = x.trim_end().parse().unwrap();
            let end = y.trim_end().parse().unwrap();
            (start..end).filter(|n| {
                let s = n.to_string();
                let (a, b) = s.split_at(s.len() / 2);
                a == b
            })
        })
        .flatten()
        .sum();

    println!("Total sum: {}", res);
}

fn read_file() -> String {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
    }
    fs::read_to_string(&args[1]).expect("Failed to read input file")
}
