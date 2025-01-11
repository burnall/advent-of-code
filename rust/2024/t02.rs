mod util;

fn parse_input(file_name: &str) -> Vec<String> {
    util::read_lines(file_name)
}

fn parse_level(raw_level: &String) -> Vec<i32> {
    raw_level.split(" ")
        .map(|part| part.parse::<i32>().unwrap())
            .collect()
}

fn grad(a: i32, b: i32) -> i32 {
    let diff = b - a;
    if diff == 0 {
        0
    } else if diff > 0 {
        1
    } else {
        -1        
    }    
}

fn check_level(level: &Vec<i32>) -> bool {
    let sign = grad(level[0], level[1]);
    for i in 0..level.len() - 1 {
        let g = grad(level[i], level[i + 1]);
        let diff = (level[i] - level[i + 1]).abs();
        if sign != g || diff == 0 || diff > 3 {
            return false;
        }
    }
    true
}


fn remove_index(level: &Vec<i32>, index: usize) -> Vec<i32> {
    let mut l = level.clone();
    l.remove(index);
    l
}

// 1 3 2 4 5: Safe by removing the second level, 3.
// 8 6 4 4 1: Safe by removing the third level, 4.
// 1 5 4
fn check_level_dampened(level: &Vec<i32>) -> bool {
    let sign = grad(level[0], level[1]);
    for i in 0..level.len() - 1 {
        let g = grad(level[i], level[i + 1]);
        let diff = (level[i] - level[i + 1]).abs();
        if sign != g || diff == 0 || diff > 3 {
            return i > 0 && check_level(&remove_index(level, i - 1)) || 
                check_level(&remove_index(level, i)) || check_level(&remove_index(level, i + 1));
        }
    }
    true
}

fn part1(raw_levels: &Vec<String>) {
    let count = raw_levels
        .into_iter()
        .map(parse_level)
        .filter(check_level_dampened)
        .count();
    println!("Part 1: {:?}", count);

    let level = vec![1, 6, 4, 2, 10];
    println!("Test: {:?}", check_level_dampened(&level));
}

fn main() {
    let levels = parse_input("data/t02.txt");
    part1(&levels);
}
