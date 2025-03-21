use crate::util;

fn parse_input(file_name: &str) -> Vec<Vec<char>> {
    util::read_lines(file_name)
        .into_iter()
        .map(|s| s.chars().collect())
        .collect()
}

fn check_direction(matrix: &Vec<Vec<char>>, p: (usize, usize), dir: &(i32, i32)) -> bool {
    let px = p.0 as i32;
    let py = p.1 as i32;
    let endp = (px + 3 * dir.0, py + 3 * dir.1);
    let is_in = endp.0 >= 0 && endp.0 < matrix[0].len() as i32 &&
        endp.1 >= 0 && endp.1 < matrix.len() as i32;
    is_in && matrix[(py + dir.1) as usize][(px + dir.0) as usize] == 'M'   
        && matrix[(py + 2 * dir.1) as usize][(px + 2 * dir.0) as usize] == 'A'
        && matrix[(py + 3 * dir.1) as usize][(px + 3 * dir.0) as usize] == 'S'
}

pub fn task1() {
    let matrix = parse_input("../data/t04.txt");
    // println!("{:?}", matrix);
    let dirs = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)];

    let mut cnt = 0;
    for x in 0..matrix[0].len() {
        for y in 0..matrix.len() {
            if matrix[y][x] == 'X' {
                for dir in dirs {
                    if check_direction(&matrix, (x, y), &dir) {
                        cnt += 1;
                    }
                }
            } 
        }    
    }
    println!("Answer is {:?}", cnt);
}

fn check_xmas(matrix: &Vec<Vec<char>>, p: (usize, usize)) -> bool {
    let x = p.0 as i32;
    let y = p.1 as i32;
    let a = matrix[(y - 1) as usize][(x - 1) as usize];
    let b = matrix[(y - 1) as usize][(x + 1) as usize];
    let c = matrix[(y + 1) as usize][(x + 1) as usize];
    let d = matrix[(y + 1) as usize][(x - 1) as usize];
    (a == 'M' && c == 'S' || a == 'S' && c == 'M') &&
    (b == 'M' && d == 'S' || b == 'S' && d == 'M')

}    

pub fn task2() {
    let matrix = parse_input("../data/t04.txt");
    let mut cnt = 0;

    for x in 1..matrix[0].len() - 1 {
        for y in 1..matrix.len() - 1 {
            if matrix[y][x] == 'A' {
                if check_xmas(&matrix, (x, y)) {
                    cnt += 1;
                }
            } 
        }    
    }
    println!("Answer is {:?}", cnt);
}