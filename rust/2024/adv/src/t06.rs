use crate::util;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
enum Dir {
    Up,
    Right,
    Down,
    Left,
}

const NEXT_DIR: [Dir; 4] = [Dir::Right, Dir::Down, Dir::Left, Dir::Up];
impl Dir {
    fn next(&self) -> Dir {
        NEXT_DIR[*self as usize].clone()
    }
}

impl Dir {
    fn to_vector(&self) -> (i32, i32) {
        match self {
            Dir::Up => (0, -1),
            Dir::Right => (1, 0),
            Dir::Down => (0, 1),
            Dir::Left => (-1, 0),
        }
    }
}

#[derive(Debug)]
struct Guard {
    pos: (i32, i32),
    dir: Dir
}

fn find_guard(map: &Vec<Vec<char>>) -> Guard {
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == '^' {
                return Guard {
                    pos: (x as i32, y as i32),
                    dir: Dir::Up
                }
            }
        }
    }
    panic!("Guard not found");
}

fn parse_input(file_name: &str) -> (Vec<Vec<char>>, Guard) {
    let map = util::read_lines(file_name)
       .iter()
       .map(|s| s.chars().collect())
       .collect();
    let guard = find_guard(&map);
    (map, guard)
}

fn walk_and_count(map: &Vec<Vec<char>>, guard: &Guard) -> usize {
    let max_y = map.len() as i32; 
    let max_x = map[0].len() as i32; 
    let (mut x, mut y) = guard.pos;
    let mut dir = guard.dir;
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    visited.insert((x, y));
    loop {
        let (dir_x, dir_y) = dir.to_vector();
        let (x1, y1) = (x + dir_x, y + dir_y);
        if x1 < 0 || x1 >= max_x || y1 < 0 || y1 >= max_y {
            return visited.len();
        }    
        if map[y1 as usize][x1 as usize] == '#' {
            dir = dir.next();    
            continue;
        }
        (x, y) = (x1, y1);
        visited.insert((x, y));
        // println!("({}, {})", x, y);
    }
}

pub fn task1() {
   let (map, guard) = parse_input("../data/t06.txt");
   let cnt = walk_and_count(&map, &guard);
   println!("{:?}", cnt);
}
