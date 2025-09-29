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

pub fn task2() {
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

    let cheats = find_all_cheats20(&map, &scores.keys().cloned().collect());
    let count = cheats
        .iter()
        .map(|(from, to)| match scores_back.get(to) {
            Some(score) => Some(score + scores.get(from).unwrap() + (distance(from, to)) as i32),
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

fn distance(a: &Point, b: &Point) -> usize {
    (a.0 as i32 - b.0 as i32).abs() as usize + (a.1 as i32 - b.1 as i32).abs() as usize
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

fn find_all_cheats20(map: &Map, points: &Vec<Point>) -> Vec<(Point, Point)> {
    let pairs = points
        .iter()
        .flat_map(|point| {
            find_nb20(map, point)
                .into_iter()
                .map(|p| (point.clone(), p))
        })
        .collect();
    pairs
}

fn find_nb20(map: &Map, point: &Point) -> Vec<Point> {
    let mut nbs = vec![];
    let (x0, y0) = (point.0 as i32, point.1 as i32);
    let min_x = 0.max(x0 - 20) as usize;
    let max_x = (map[0].len() as i32).min(x0 + 21) as usize;
    let min_y = 0.max(y0 - 20) as usize;
    let max_y = (map.len() as i32).min(y0 + 21) as usize;
    for x in min_x..max_x as usize {
        for y in min_y..max_y {
            if map[y][x] != '#' && (x0 - x as i32).abs() + (y0 - y as i32).abs() <= 20 {
                nbs.push(Point(x as usize, y as usize));
            }
        }
    }

    nbs
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
