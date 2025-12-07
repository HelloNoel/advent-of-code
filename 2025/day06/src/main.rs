use utils;

fn main() {
    let content = utils::read_file();

    let homework: Vec<Vec<&str>> = content
        .lines()
        .map(|x| x.split_whitespace().collect())
        .collect();

    let problem_num = homework[0].len();
    let problem_len = homework.len() - 1;

    let res1: u64 = (0..problem_num)
        .map(|n| {
            let nums = (0..problem_len).map(|i| homework[i][n].parse::<u64>().unwrap());
            match homework[problem_len][n] {
                "*" => nums.product::<u64>(),
                "+" => nums.sum(),
                _ => panic!("Invalid Operator"),
            }
        })
        .sum();

    println!("Part 1: {}", res1);

    let mut homework_iters: Vec<_> = content
        .lines()
        .take(problem_len)
        .map(|x| x.chars())
        .collect();
    let homework_transposed: Vec<_> = (0..content.lines().next().unwrap().len())
        .map(|_| {
            homework_iters
                .iter_mut()
                .map(|c| c.next().unwrap())
                .collect::<String>()
                .trim()
                .parse::<u64>()
        })
        .collect();

    let mut homework_iter = homework_transposed.iter().peekable();

    let res2: u64 = (0..problem_num)
        .map(|n| {
            let nums: Vec<_> = homework_iter
                .by_ref()
                .take_while(|x| x.is_ok())
                .map(|x| x.as_ref().unwrap())
                .collect();

            match homework[problem_len][n] {
                "*" => nums.into_iter().product::<u64>(),
                "+" => nums.into_iter().sum(),
                _ => panic!("Invalid Operator"),
            }
        })
        .sum();

    println!("Part 2: {}", res2);
}
