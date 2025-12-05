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
            let mut bank = &line
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<u32>>()[..];

            let mut res = 0;
            let mut mul = 10u64.pow(u32::try_from(num).unwrap() - 1);
            let mut end = bank.len() - num + 1;
            for _ in 0..num {
                let m = bank[0..end].iter().max().unwrap();
                let p = bank.iter().position(|x| x == m).unwrap();
                res += u64::from(*m) * mul;
                bank = &bank[p + 1..];
                end -= p;
                mul /= 10;
            }
            res
        })
        .sum()
}
