use std::env;
use std::fs;
use std::process;

pub fn read_file() -> String {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        process::exit(1);
    }
    fs::read_to_string(&args[1]).expect("Failed to read input file")
}
