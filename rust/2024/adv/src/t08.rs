use crate::util;
use std::collections::HashMap;
use std::collections::HashSet;

fn parse_input(file_name: &str) -> Vec<Vec<char>> {
    util::read_lines(file_name)
       .iter()
       .map(|s| s.chars().collect())
       .collect()
}

fn find_antennas(map: &Vec<Vec<char>>) -> HashMap<char, Vec<(usize, usize)>> {
    let mut hash = HashMap::new();
    for y in 0..map.len() {
       for x in 0..map[y].len() {
           let ch = map[y][x];
           if ch != '.' {
               hash.entry(ch)
                  .and_modify(|antennas: &mut Vec<_>| antennas.push((x, y)))
                  .or_insert(vec![(x, y)]); 
           }
       }
    }
    hash
}

fn get_antinodes(a: (usize, usize), b: (usize, usize)) -> Vec<(i32, i32)> {
    let (ax, ay) = (a.0 as i32, a.1 as i32);
    let (bx, by) = (b.0 as i32, b.1 as i32);
    let dx = ax - bx;
    let dy = ay - by;
    vec![(ax + dx, ay + dy), (bx - dx, by - dy)]
}

fn get_all_antinodes(antennas: &Vec<(usize, usize)>) -> Vec<(i32, i32)> {
    let mut antinodes = Vec::new();
    for j in 1..antennas.len() {
         for i in 0..j {
             get_antinodes(antennas[i], antennas[j]).iter()
                 .for_each(|a| antinodes.push(*a));
         }  
    }
    antinodes
}

pub fn task1() {
    let map = parse_input("../data/t08.txt");
    let antennas = find_antennas(&map);
    let max_y = map.len() as i32; 
    let max_x = map[0].len() as i32; 
    let count = antennas.values()
       .flat_map(get_all_antinodes)
       .filter(|&(x, y)| x >= 0 && x < max_x && y >= 0 && y < max_y)
       .collect::<HashSet<_>>()
       .len();

   println!("{:?}", count);
}

fn get_antinodes2(a: (usize, usize), b: (usize, usize), max: (i32, i32)) -> Vec<(i32, i32)> {
    let (ax, ay) = (a.0 as i32, a.1 as i32);
    let (bx, by) = (b.0 as i32, b.1 as i32);
    let dx = ax - bx;
    let dy = ay - by;
    let mut v = vec![(ax, ay), (bx, by)];
    
    let (mut cx, mut cy) = (ax, ay);
    loop {
        cx += dx;
        cy += dy;
        if cx < 0 || cx >= max.0 || cy < 0 || cy >= max.1 {
            break;   
        }
        v.push((cx, cy));
    }

    (cx, cy) = (bx, by);
    loop {
        cx -= dx;
        cy -= dy;
        if cx < 0 || cx >= max.0 || cy < 0 || cy >= max.1 {
            break;   
        }
        v.push((cx, cy));
    }

    v
}

fn get_all_antinodes2(antennas: &Vec<(usize, usize)>, max: (i32, i32)) -> Vec<(i32, i32)> {
    let mut antinodes = Vec::new();
    for j in 1..antennas.len() {
         for i in 0..j {
             get_antinodes2(antennas[i], antennas[j], max).iter()
                 .for_each(|a| antinodes.push(*a));
         }  
    }
    antinodes
}

pub fn task2() {
    let map = parse_input("../data/t08.txt");
    let antennas = find_antennas(&map);
    let max_y = map.len() as i32; 
    let max_x = map[0].len() as i32; 
    let count = antennas.values()
       .flat_map(|ans| get_all_antinodes2(ans, (max_x, max_y)))
       .filter(|&(x, y)| x >= 0 && x < max_x && y >= 0 && y < max_y)
       .collect::<HashSet<_>>()
       .len();

   println!("{:?}", count);
}
