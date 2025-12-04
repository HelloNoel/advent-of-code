use utils;

fn main() {
    let contents = utils::read_file();
    let instructions = parse_instructions(&contents);

    let mut position = 50;
    let mut stopped_count = 0;
    let mut passed_count = 0;

    for (direction, distance) in instructions {
        let passed;
        (position, passed) = turn_dial(position, direction, distance);
        if position == 0 {
            stopped_count += 1;
        }
        passed_count += passed;
    }

    println!("Final position: {}", position);
    println!("Stopped at 0: {} times", stopped_count);
    println!("Passed 0: {} times", passed_count);
}

fn parse_instructions(contents: &str) -> Vec<(char, i32)> {
    let mut instructions = Vec::new();
    for line in contents.lines() {
        let direction = line.chars().next().unwrap();
        let distance = line[1..].parse().unwrap();
        instructions.push((direction, distance));
    }
    instructions
}

fn turn_dial(position: i32, direction: char, distance: i32) -> (i32, i32) {
    match direction {
        'L' => {
            let new_position = position - distance;
            let count = if new_position > 0 {
                0
            } else if new_position == 0 {
                1
            } else if position == 0 {
                -new_position / 100
            } else {
                (-new_position / 100) + 1
            };
            let final_position = ((new_position % 100) + 100) % 100;
            (final_position, count)
        }
        'R' => {
            let new_position = position + distance;
            let count = new_position / 100;
            let final_position = new_position % 100;
            (final_position, count)
        }
        _ => panic!("Invalid direction"),
    }
}
