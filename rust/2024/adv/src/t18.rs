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

    let start = (1, 1);
    let end = (SIZE, SIZE);
    let mut scores = HashMap::from([(start, 0)]);
    traverse(&map, &mut scores, &vec![start]);
    println!("{:?}", scores.get(&end));
}

fn traverse(map: &Map, scores: &mut HashMap<Point, i32>, points: &Vec<Point>) {
    let mut next_points = vec![];
    for point in points {
        let score = *scores.get(point).unwrap();
        for dir in [(-1, 0), (0, -1), (1, 0), (0, 1)] {
            let nb = (
                (point.0 as i32 + dir.0) as usize,
                (point.1 as i32 + dir.1) as usize,
            );
            if map[nb.1][nb.0] == '#' || scores.contains_key(&nb) {
                continue;
            }
            scores.insert(nb, score + 1);
            next_points.push(nb);
        }
    }
    if !next_points.is_empty() {
        traverse(map, scores, &next_points);
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
