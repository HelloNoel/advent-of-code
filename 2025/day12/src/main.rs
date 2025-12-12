use utils;

fn main() {
    let content = utils::read_file();

    let res1 = content
        .lines()
        .filter(|line| line.contains("x"))
        .map(|line| {
            let (size, gifts) = line.split_once(": ").unwrap();
            let (x, y) = size.split_once("x").unwrap();

            (
                (x.parse::<u32>().unwrap(), y.parse::<u32>().unwrap()),
                gifts
                    .split(" ")
                    .map(|s| s.parse().unwrap())
                    .collect::<Vec<u32>>(),
            )
        })
        .filter(|((x, y), gifts)| (x / 3) * (y / 3) >= gifts.iter().sum())
        .count();

    println!("Part 1: {}", res1);
}
