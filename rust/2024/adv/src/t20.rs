use crate::util;
use std::collections::{HashMap, HashSet};

type Map = Vec<Vec<char>>;
#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Point(usize, usize);

pub fn task1() {
    let mut map = parse_map("../data/t20.txt");
    add_border(&mut map);

    let start = get_pos(&map, 'S');
    let end = get_pos(&map, 'E');
    //print_map(&map);

    let mut scores = HashMap::from([(start.clone(), 0)]);
    let points = HashSet::from([start.clone()]);
    traverse(&map, &mut scores, &points);
    let fair_score = scores.get(&end).unwrap();
    println!("Fair score: {}", fair_score);

    let mut scores_back = HashMap::from([(end.clone(), 0)]);
    let points_back = HashSet::from([end]);
    traverse(&map, &mut scores_back, &points_back);
    let fair_score = scores_back.get(&start).unwrap();

    let cheats = find_all_cheats(&map, &scores.keys().cloned().collect());
    let count = cheats
        .iter()
        .map(|(from, to)| match scores_back.get(to) {
            Some(score) => Some(score + scores.get(from).unwrap() + 2),
            None => None,
        })
        .filter(|opt| {
            if let Some(score) = opt {
                if *score <= *fair_score - 100 {
                    return true;
                }
            }
            false
        })
        .count();

    println!("Number of 100+ cheats: {}", count);
}

fn traverse(map: &Map, scores: &mut HashMap<Point, i32>, start_points: &HashSet<Point>) {
    let mut points = start_points.clone();

    loop {
        let mut next_points = HashSet::new();
        for point in &points {
            let score = *scores.get(&point).unwrap();
            for dir in [(-1, 0), (0, -1), (1, 0), (0, 1)] {
                let nb = Point(
                    (point.0 as i32 + dir.0) as usize,
                    (point.1 as i32 + dir.1) as usize,
                );

                if map[nb.1][nb.0] == '#' || scores.contains_key(&nb) {
                    continue;
                }

                scores.insert(nb.clone(), score + 1);
                next_points.insert(nb);
            }
        }
        if next_points.is_empty() {
            break;
        }
        points = next_points;
    }
}

fn find_all_cheats(map: &Map, points: &Vec<Point>) -> Vec<(Point, Point)> {
    let mut pairs = vec![];
    for point in points {
        for dir in [(-1, 0), (0, -1), (1, 0), (0, 1)] {
            let nb = Point(
                (point.0 as i32 + dir.0) as usize,
                (point.1 as i32 + dir.1) as usize,
            );
            if map[nb.1][nb.0] == '#' {
                let p = Point(
                    (nb.0 as i32 + dir.0) as usize,
                    (nb.1 as i32 + dir.1) as usize,
                );
                if map[p.1][p.0] != '#' {
                    pairs.push((point.clone(), p));
                }
            }
        }
    }
    pairs
}

fn parse_map(file_name: &str) -> Map {
    util::read_lines(file_name)
        .iter()
        .map(|line| line.chars().collect())
        .collect()
}

fn get_pos(map: &Map, ch: char) -> Point {
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == ch {
                return Point(x, y);
            }
        }
    }
    panic!("{} not found", ch);
}

fn add_border(map: &mut Map) {
    let max_y = map.len();
    for row in 0..max_y {
        map[row].insert(0, '#');
        map[row].push('#');
    }
    let max_x = map[0].len();
    let line = vec!['#'; max_x];
    map.insert(0, line.clone());
    map.push(line);
}

fn print_map(map: &Map) {
    for row in map {
        let line: String = row.iter().collect();
        println!("{}", line);
    }
}
