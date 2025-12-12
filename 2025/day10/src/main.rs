use std::collections::VecDeque;

use good_lp::{Expression, Solution, SolverModel, default_solver, variable, variables};
use std::iter::repeat;
use utils;

fn main() {
    let content = utils::read_file();

    let parts: Vec<_> = content
        .lines()
        .map(|line| line.split(" ").collect::<Vec<_>>())
        .collect();

    let lights: Vec<_> = parts
        .iter()
        .map(|line| {
            let mut l = line[0].chars();
            l.next();
            l.next_back();
            l.map(|c| c == '#').collect::<Vec<_>>()
        })
        .collect();

    let buttons: Vec<_> = parts
        .iter()
        .map(|line| {
            line[1..line.len() - 1]
                .into_iter()
                .map(|b| {
                    let mut b = b.chars();
                    b.next();
                    b.next_back();
                    b.collect::<String>()
                        .split(",")
                        .map(|n| n.parse::<usize>().unwrap())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let rows: Vec<_> = lights.iter().zip(buttons.iter()).collect();

    let res1: u64 = rows
        .iter()
        .map(|(target_lights, buttons)| {
            let mut queue = VecDeque::new();
            queue.push_back((repeat(false).take(target_lights.len()).collect(), 0));
            loop {
                let (lights, pressed): (Vec<_>, _) = queue.pop_front().unwrap();
                let new_lights: Vec<_> = buttons
                    .iter()
                    .map(|bs| {
                        let mut l = lights.clone();
                        for b in bs.iter() {
                            l[*b] = !l[*b];
                        }
                        l
                    })
                    .collect();
                if new_lights.iter().any(|l| l == *target_lights) {
                    return pressed + 1;
                }
                queue.extend(new_lights.into_iter().map(|l| (l, pressed + 1)));
            }
        })
        .sum();

    println!("Part 1: {}", res1);

    let joltage: Vec<_> = parts
        .into_iter()
        .map(|line| {
            let mut l = line[line.len() - 1].chars();
            l.next();
            l.next_back();
            l.collect::<String>()
                .split(",")
                .map(|n| n.parse::<u32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect();

    let res2: u64 = joltage
        .iter()
        .zip(buttons.iter())
        .map(|(target_jolts, buttons)| solve(target_jolts, buttons).round() as u64)
        .sum();

    println!("Part 2: {}", res2);
}

fn solve(target_jolts: &Vec<u32>, buttons: &Vec<Vec<usize>>) -> f64 {
    let num_vars = buttons.len();
    let num_constraints = target_jolts.len();

    let mut vars = variables!();
    let vars_vec: Vec<_> = (0..num_vars)
        .map(|_| vars.add(variable().integer().min(0)))
        .collect();

    let objective = vars_vec
        .iter()
        .fold(Expression::default(), |expr, var| expr + var);

    let mut problem = vars.minimise(objective).using(default_solver);

    problem = (0..num_constraints).fold(problem, |acc, j| {
        acc.with(
            buttons
                .iter()
                .enumerate()
                .fold(Expression::default(), |expr, (i, bs)| {
                    if bs.contains(&j) {
                        expr + &vars_vec[i]
                    } else {
                        expr
                    }
                })
                .eq(target_jolts[j]),
        )
    });

    let solution = problem.solve().unwrap();

    (0..num_vars).map(|i| solution.value(vars_vec[i])).sum()
}
