use crate::util;
use std::collections::HashMap;
use std::collections::HashSet;

fn parse_input(file_name: &str) -> Vec<Vec<char>> {
    util::read_lines(file_name)
       .iter()
       .map(|s| s.chars().collect())
       .collect()
}

fn findAntennas(map: &Vec<Vec<char>>) -> HashMap<char, Vec<(usize, usize)>> {
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

fn getAntinodes(a: (usize, usize), b: (usize, usize)) -> Vec<(i32, i32)> {
    let (ax, ay) = (a.0 as i32, a.1 as i32);
    let (bx, by) = (b.0 as i32, b.1 as i32);
    let dx = (ax - bx).abs();
    let dy = (ay - by).abs();
    vec![(if ax < bx {ax - dx} else {ax + dx}, if ay < by {ay - dy} else {ay + dy}), 
         (if bx < ax {bx - dx} else {bx + dx}, if by < ay {by - dy} else {by + dy}) 
    ]
}

fn getAllAntinodes(antennas: &Vec<(usize, usize)>) -> Vec<(i32, i32)> {
    let mut antinodes = Vec::new();
    for j in 1..antennas.len() {
         for i in 0..j {
             getAntinodes(antennas[i], antennas[j]).iter()
                 .for_each(|a| antinodes.push(*a));
         }  
    }
    antinodes
}

pub fn task1() {
    let map = parse_input("../data/t08.txt");
    let antennas = findAntennas(&map);
    let max_y = map.len() as i32; 
    let max_x = map[0].len() as i32; 
    let count = antennas.values()
       .flat_map(getAllAntinodes)
       .filter(|&(x, y)| x >= 0 && x < max_x && y >= 0 && y < max_y)
       .collect::<HashSet<_>>()
       .len();

   println!("{:?}", count);
}