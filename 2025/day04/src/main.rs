use utils;

const NEIGHBOURS: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn main() {
    let content = utils::read_file();

    let grid = content
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let res1: usize = (0..grid.len())
        .map(|i| {
            (0..grid[i].len())
                .filter(|j| is_accessible(&grid, i, *j))
                .count()
        })
        .sum();

    println!("Part 1: {}", res1);

    let mut grid = grid;
    let mut res2 = 0;
    let mut removed = 1;
    while removed != 0 {
        let removable: Vec<(usize, usize)> = (0..grid.len())
            .flat_map(|i| {
                (0..grid[i].len())
                    .filter(|&j| is_accessible(&grid, i, j))
                    .map(move |j| (i, j))
                    .collect::<Vec<_>>()
            })
            .collect();
        removed = removable.iter().count();
        res2 += removed;
        removable.into_iter().for_each(|(i, j)| {
            grid[i][j] = 'x';
        });
    }

    println!("Part 2: {}", res2);
}

fn is_accessible(grid: &Vec<Vec<char>>, x: usize, y: usize) -> bool {
    if grid[x][y] != '@' {
        return false;
    }
    NEIGHBOURS
        .iter()
        .filter(|(dx, dy)| {
            let ni = usize::try_from(x as isize + dx);
            let nj = usize::try_from(y as isize + dy);
            match ni {
                Ok(ni) => {
                    ni < grid.len()
                        && match nj {
                            Ok(nj) => nj < grid[ni].len() && grid[ni][nj] == '@',
                            Err(_) => false,
                        }
                }
                Err(_) => false,
            }
        })
        .count()
        < 4
}
