use crate::util;
use std::{collections::HashMap, vec};

type Map = Vec<Vec<char>>;
type Point = (usize, usize);

pub fn task1() {
    const SIZE: usize = 71;
    let pairs = read_pairs("../data/t18.txt");
    let mut map = get_initial_map(71);
    add_walls(&mut map, &pairs[0..1024]);
    add_border(&mut map);
    let is_wall = |point: &Point| map[point.1][point.0] == '#';

    let start = (1, 1);
    let end = (SIZE, SIZE);
    let mut scores = HashMap::from([(start, 0)]);
    traverse(is_wall, &mut scores, &vec![start]);
    println!("{:?}", scores.get(&end));
}

pub fn task2() {
    const SIZE: usize = 71;
    let pairs = read_pairs("../data/t18.txt");
    let falls = pairs.iter()
        .enumerate()
        .map(|(index, &pair)| (pair, index))
        .collect::<HashMap<_, _>>();
    let start = (0, 0);
    let end = (SIZE - 1, SIZE - 1);

    let is_reachable = |bytes| {
        let mut scores = HashMap::from([(start, 0)]);
        let is_wall = |point: &Point| {
            if point.0 >= SIZE || point.1 >= SIZE {
                return true;
            }
            match falls.get(point) {
                Some(&index) => index < bytes,
                None => false,
            }
        };
        traverse(is_wall, &mut scores, &vec![start]);
        scores.contains_key(&end)
    };
        
    let first_failing_byte = binary_search(0, falls.len(), is_reachable) - 1;
    println!("{:?} {:?}", first_failing_byte, pairs[first_failing_byte]);
}


fn binary_search<F>(low: usize, high: usize, func: F) -> usize
where
    F: Fn(usize) -> bool,
{
    let mut low = low;
    let mut high = high;
    while low < high {
        let mid = (low + high) / 2;
        if func(mid) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    low
}


fn traverse<F>(is_wall: F, scores: &mut HashMap<Point, i32>, points: &Vec<Point>) 
where
    F: Fn(&Point) -> bool {
    let mut next_points = vec![];
    for point in points {
        let score = *scores.get(point).unwrap();
        for dir in [(-1, 0), (0, -1), (1, 0), (0, 1)] {
            let nb = (
                (point.0 as i32 + dir.0) as usize,
                (point.1 as i32 + dir.1) as usize,
            );
            if is_wall(point) || scores.contains_key(&nb) {
                continue;
            }
            scores.insert(nb, score + 1);
            next_points.push(nb);
        }
    }
    if !next_points.is_empty() {
        traverse(is_wall, scores, &next_points);
    }
}

fn read_pairs(file_name: &str) -> Vec<(usize, usize)> {
    util::read_lines(file_name)
        .into_iter()
        .map(|line| {
            let parts: Vec<usize> = line
                .split(",")
                .map(|part| part.parse::<usize>().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .collect::<Vec<(usize, usize)>>()
}

fn get_initial_map(size: usize) -> Vec<Vec<char>> {
    vec![vec!['.'; size]; size]
}

fn add_walls(map: &mut Map, pairs: &[(usize, usize)]) {
    for (x, y) in pairs {
        map[*y][*x] = '#';
    }
}

fn add_border(map: &mut Map) {
    let size = map.len();
    let line = vec!['#'; size + 2];
    for row in 0..size {
        map[row].insert(0, '#');
        map[row].push('#');
    }
    map.insert(0, line.clone());
    map.push(line);
}

fn draw(map: &Map, scores: &HashMap<Point, i32>) {
    for y in 0..map.len() {
        let text = (0..map[y].len())
            .map(|x| {
                if map[y][x] == '#' {
                    "#".to_string()
                } else {
                    scores.get(&(x, y)).unwrap_or(&0).to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(",");
        println!("{:?}", text);
    }
}
