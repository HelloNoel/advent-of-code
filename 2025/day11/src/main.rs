use std::collections::HashMap;
use utils;

fn main() {
    let content = utils::read_file();

    let connections: HashMap<_, _> = content
        .lines()
        .map(|line| {
            let (key, rest) = line.split_once(": ").unwrap();
            let value: Vec<_> = rest.split(" ").collect();
            (key, value)
        })
        .collect();

    println!(
        "Part 1: {}",
        count_paths(&connections, &mut HashMap::new(), "you", "out")
    );

    println!("Part 2: {}", dac_fft_paths(&connections))
}

fn count_paths<'a>(
    connections: &HashMap<&str, Vec<&'a str>>,
    visited: &mut HashMap<&'a str, u64>,
    device: &str,
    destination: &str,
) -> u64 {
    if device == destination {
        return 1;
    }

    match connections.get(device) {
        Some(d) => d
            .iter()
            .map(|next_device| match visited.get(next_device) {
                Some(i) => *i,
                None => {
                    let next_paths = count_paths(connections, visited, next_device, destination);
                    visited.insert(next_device, next_paths);
                    next_paths
                }
            })
            .sum(),
        None => 0,
    }
}

fn dac_fft_paths<'a>(connections: &HashMap<&'a str, Vec<&'a str>>) -> u64 {
    count_paths(&connections, &mut HashMap::new(), "svr", "fft")
        * count_paths(&connections, &mut HashMap::new(), "fft", "dac")
        * count_paths(&connections, &mut HashMap::new(), "dac", "out")
        + count_paths(&connections, &mut HashMap::new(), "svr", "dac")
            * count_paths(&connections, &mut HashMap::new(), "dac", "fft")
            * count_paths(&connections, &mut HashMap::new(), "fft", "out")
}
