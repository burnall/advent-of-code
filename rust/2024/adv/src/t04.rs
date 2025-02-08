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