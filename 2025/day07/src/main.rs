use std::collections::HashMap;
use std::collections::HashSet;
use utils;

fn main() {
    let content = utils::read_file();

    let mut input = content.lines();

    let start: HashSet<usize> = input
        .next()
        .unwrap()
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == 'S')
        .map(|(i, _)| i)
        .collect();

    let (_, res1) = input.clone().fold((start.clone(), 0), |(acc, i), line| {
        let mut new_beams = HashSet::new();
        let mut new_splits = i;
        line.chars().enumerate().for_each(|(i, c)| {
            if acc.contains(&i) {
                if c == '^' {
                    new_beams.insert(i - 1);
                    new_beams.insert(i + 1);
                    new_splits += 1;
                } else {
                    new_beams.insert(i);
                }
            }
        });
        (new_beams, new_splits)
    });

    println!("Part 1: {}", res1);

    let start: HashMap<_, u64> = start.into_iter().map(|c| (c, 1)).collect();

    let res2: u64 = input
        .fold(start, |beams, line| {
            let mut new_beams = HashMap::new();
            line.chars()
                .enumerate()
                .for_each(|(i, c)| match beams.get(&i) {
                    Some(&count) => {
                        if c == '^' {
                            *new_beams.entry(i - 1).or_insert(0) += count;
                            *new_beams.entry(i + 1).or_insert(0) += count;
                        } else {
                            *new_beams.entry(i).or_insert(0) += count;
                        }
                    }
                    None => {}
                });
            new_beams
        })
        .values()
        .sum();

    println!("Part 2: {}", res2);
}
