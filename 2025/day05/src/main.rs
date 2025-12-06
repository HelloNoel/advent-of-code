use utils;

fn main() {
    let content = utils::read_file();
    let (top, bottom) = content.split_once("\n\n").unwrap();

    let mut fresh: Vec<(u64, u64)> = top
        .lines()
        .map(|x| {
            let (start, end) = x.split_once("-").unwrap();
            (start.parse().unwrap(), end.parse().unwrap())
        })
        .collect();

    merge_ranges(&mut fresh);

    let res1 = bottom
        .lines()
        .map(|x| x.parse().unwrap())
        .filter(|x: &u64| fresh.iter().any(|(min, max)| x >= min && x <= max))
        .count();

    println!("Part 1: {}", res1);

    let res2: u64 = fresh.into_iter().map(|(x, y)| y - x + 1).sum();

    println!("Part 2: {}", res2)
}

fn merge_ranges(r: &mut Vec<(u64, u64)>) {
    let mut i = 0;
    while i < r.len() {
        let mut j = i + 1;
        let mut merged = false;
        while j < r.len() {
            let (min_i, max_i) = r[i];
            let (min_j, max_j) = r[j];
            if max_i >= min_j && max_j >= min_i {
                r[i] = (min_i.min(min_j), max_i.max(max_j));
                r.swap_remove(j);
                merged = true;
            }
            j += 1;
        }
        if !merged {
            i += 1;
        }
    }
}
