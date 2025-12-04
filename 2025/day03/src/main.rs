use utils;

fn main() {
    let contents = utils::read_file();

    let res1: u32 = contents
        .lines()
        .map(|line| {
            let bank: Vec<u32> = line.chars().map(|c| c.to_digit(10).unwrap()).collect();
            let l = bank.len();

            let i = l - 2 - argmax(&bank[..l - 1].iter().rev());
            let j = argmax(&bank[i + 1..].iter());

            (bank[i].to_string() + &bank[i + j + 1].to_string())
                .parse::<u32>()
                .unwrap()
        })
        .sum();

    println!("Part 1: {}", res1);
}

fn argmax(v: &(impl Iterator<Item = impl Ord + Copy> + Clone)) -> usize {
    v.clone()
        .enumerate()
        .max_by_key(|&(_, val)| val)
        .map(|(idx, _)| idx)
        .unwrap()
}
