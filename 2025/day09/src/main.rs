use rayon::prelude::*;
use tqdm::Iter;
use utils;

fn main() {
    let content = utils::read_file();

    let tiles: Vec<_> = content
        .lines()
        .map(|line| line.split_once(",").unwrap())
        .map(|(x, y)| (x.parse::<i64>().unwrap(), y.parse::<i64>().unwrap()))
        .collect();

    let max_area = tiles
        .iter()
        .enumerate()
        .flat_map(|(i, (x1, y1))| {
            tiles[i + 1..]
                .iter()
                .map(|(x2, y2)| ((x2 - x1.clone()).abs() + 1) * ((y2 - y1.clone()).abs() + 1))
        })
        .max()
        .unwrap();

    println!("Max rectangle area: {}", max_area);

    let mut boundery_tiles = tiles.clone();
    boundery_tiles.push(tiles[0].clone());

    let green_boundry: Vec<_> = boundery_tiles
        .windows(2)
        .map(|w| <&[(i64, i64); 2]>::try_from(w).unwrap())
        .collect();

    let mut rectangles: Vec<_> = tiles
        .iter()
        .enumerate()
        .flat_map(|(i, (x1, y1))| {
            tiles[i + 1..].iter().map(|(x2, y2)| {
                let area = ((x2 - x1.clone()).abs() + 1) * ((y2 - y1.clone()).abs() + 1);
                ((x1.clone(), y1.clone()), (x2, y2), area)
            })
        })
        .collect();

    rectangles.sort_by(|(_, _, x), (_, _, y)| x.cmp(y));

    let (_, _, largest_area) = rectangles
        .into_iter()
        .rev()
        .tqdm()
        .filter(|((x1, y1), (x2, y2), _)| {
            let x_lower = *x1.min(x2);
            let x_upper = *x1.max(x2);
            let y_lower = *y1.min(y2);
            let y_upper = *y1.max(y2);

            let top = (x_lower..=x_upper).into_par_iter().map(|x| (x, y_lower));
            let right = (y_lower + 1..y_upper).into_par_iter().map(|y| (x_upper, y));
            let bottom = (x_lower..=x_upper).into_par_iter().map(|x| (x, y_upper));
            let left = (y_lower + 1..y_upper).into_par_iter().map(|y| (x_lower, y));

            top.chain(right)
                .chain(bottom)
                .chain(left)
                .all(|(x, y)| point_in_polygon(&green_boundry, (x, y)))
        })
        .next()
        .unwrap();

    println!("Largest rectangle in green area: {}", largest_area);
}

fn point_in_polygon(green_boundry: &[&[(i64, i64); 2]], (px, py): (i64, i64)) -> bool {
    let mut intersections: u64 = 0;

    for line in green_boundry.iter() {
        let [(x1, y1), (x2, y2)] = line;

        if (x1 == x2 && px == *x1 && py >= *y1.min(y2) && py <= *y1.max(y2))
            || (y1 == y2 && py == *y1 && px >= *x1.min(x2) && px <= *x1.max(x2))
        {
            return true;
        }

        if (py < *y1) != (py < *y2) {
            let intersect_x = x1 + (py - y1) * (x2 - x1) / (y2 - y1);
            if intersect_x >= px {
                intersections += 1;
            }
        }
    }

    intersections % 2 == 1
}
