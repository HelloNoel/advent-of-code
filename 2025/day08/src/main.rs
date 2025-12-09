use itertools::Itertools;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input_file> <connections>", args[0]);
        process::exit(1);
    }

    let content = fs::read_to_string(&args[1]).expect("Failed to read input file");

    let max_connections = args[2]
        .parse()
        .expect("Failed to parse number of connections");

    let positions: Vec<_> = content
        .lines()
        .map(|line| {
            line.split(",")
                .map(|num| num.parse().unwrap())
                .collect_tuple::<(i64, i64, i64)>()
                .unwrap()
        })
        .collect();

    let distances = positions
        .iter()
        .enumerate()
        .fold(BTreeMap::new(), |mut acc, (idx, pos)| {
            for other_pos in positions.iter().skip(idx + 1) {
                let distance = (pos.0 - other_pos.0).pow(2)
                    + (pos.1 - other_pos.1).pow(2)
                    + (pos.2 - other_pos.2).pow(2);

                acc.entry(distance)
                    .or_insert(Vec::new())
                    .push((pos, other_pos));
            }
            acc
        });

    let (mut circuits, _) = add_conections(&distances, Some(max_connections), positions.len());

    circuits.sort_by(|a, b| a.len().cmp(&b.len()));

    let res1: usize = circuits.iter().rev().take(3).map(|c| c.len()).product();

    println!("Part 1: {}", res1);

    let (_, connections) = add_conections(&distances, None, positions.len());

    let res2 = distances
        .values()
        .flatten()
        .skip(connections - 1)
        .next()
        .unwrap();

    println!("Part 2: {}", res2.0.0 * res2.1.0);
}

fn add_conections(
    distances: &BTreeMap<i64, Vec<(&(i64, i64, i64), &(i64, i64, i64))>>,
    max_connections: Option<usize>,
    position_count: usize,
) -> (Vec<HashSet<(i64, i64, i64)>>, usize) {
    let mut circuits: Vec<HashSet<(i64, i64, i64)>> = Vec::new();
    let mut connections: usize = 0;

    for pairs in distances.values() {
        for (pos1, pos2) in pairs.iter() {
            let mut pos1_found = None;
            let mut pos2_found = None;
            for (i, circuit) in circuits.iter_mut().enumerate() {
                if circuit.contains(pos1) && circuit.contains(pos2) {
                    pos1_found = Some(i);
                    pos2_found = Some(i);
                    break;
                } else if circuit.contains(pos1) {
                    pos1_found = Some(i);
                } else if circuit.contains(pos2) {
                    pos2_found = Some(i);
                }
                if pos1_found.is_some() && pos2_found.is_some() {
                    break;
                }
            }
            match (pos1_found, pos2_found) {
                (Some(i), Some(j)) if i != j => {
                    let (low_idx, high_idx) = if i < j { (i, j) } else { (j, i) };
                    let mut to_merge = circuits.swap_remove(high_idx);
                    let target = &mut circuits[low_idx];
                    target.extend(to_merge.drain());
                }
                (Some(i), None) => {
                    circuits[i].insert(**pos2);
                }
                (None, Some(i)) => {
                    circuits[i].insert(**pos1);
                }
                (None, None) => {
                    let mut new_circuit = HashSet::new();
                    new_circuit.insert(**pos1);
                    new_circuit.insert(**pos2);
                    circuits.push(new_circuit);
                }
                _ => {}
            }
            connections += 1;
            if max_connections.is_some() && connections == max_connections.unwrap() {
                break;
            }
        }

        if max_connections.is_some() && connections == max_connections.unwrap() {
            break;
        }

        if circuits[0].len() == position_count {
            break;
        }
    }
    (circuits, connections)
}
