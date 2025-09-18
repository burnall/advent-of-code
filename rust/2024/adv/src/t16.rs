use crate::util;
use std::collections::HashMap;
use std::collections::HashSet;

type Map = Vec<Vec<char>>;
type Point = (i32, i32);

#[derive(Debug)]
struct Move {
    point: Point,
    dir: Point,
    cost: i32,
}

const LEFT: Point = (-1, 0);
const UP: Point = (0, -1);
const RIGHT: Point = (1, 0);
const DOWN: Point = (0, 1);

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
                return (x as i32, y as i32);
            }
        }
    }
    panic!("{} not found", ch);
}

fn draw2(map: &Map, scores: &HashMap<Point, i32>) {
    for y in 0..map.len() {
        let text = (0..map[y].len())
            .map(|x| {
                if map[y][x] == '#' {
                    "#".to_string()
                } else {
                    scores.get(&(x as i32, y as i32)).unwrap_or(&-1).to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(",");
        println!("{:?}", text);
    }
}

fn traverse(map: &Map, point_scores: &mut HashMap<Point, i32>, moves: &Vec<Move>) {
    let mut new_point_scores = HashMap::new();
    let mut point_dirs: HashMap<Point, HashSet<Point>> = HashMap::new();
    for mv in moves {
        let new_point_scores_update = get_new_points_and_scores(map, mv, point_scores);
        add_point_dirs(&mut point_dirs, &new_point_scores_update, mv.dir);
        merge_scores(&mut new_point_scores, &new_point_scores_update);
    }
    point_scores.extend(new_point_scores.into_iter());
    let new_moves = get_new_moves(&point_dirs);
    if new_moves.is_empty() {
        return;
    }
    traverse(map, point_scores, &new_moves);
}

fn get_new_points_and_scores(
    map: &Map,
    mv: &Move,
    scores: &mut HashMap<Point, i32>,
) -> HashMap<Point, i32> {
    let base_score = scores.get(&mv.point).unwrap() + mv.cost;
    let mut points = HashMap::new();
    let mut i = 0;
    let mut next_point = mv.point;
    loop {
        next_point = (next_point.0 + mv.dir.0, next_point.1 + mv.dir.1);
        i += 1;
        if scores.contains_key(&next_point) {
            return points;
        }
        if map[next_point.1 as usize][next_point.0 as usize] == '#' {
            return points;
        }
        points.insert(next_point, base_score + i);
    }
}

fn add_point_dirs(
    point_dirs: &mut HashMap<Point, HashSet<Point>>,
    point_scores: &HashMap<Point, i32>,
    dir: Point,
) {
    for &point in point_scores.keys() {
        point_dirs
            .entry(point)
            .and_modify(|dirs| {
                dirs.insert(dir);
            })
            .or_insert(HashSet::from([dir]));
    }
}

fn merge_scores(point_scores: &mut HashMap<Point, i32>, point_scores_update: &HashMap<Point, i32>) {
    for (&point, &new_score) in point_scores_update {
        point_scores
            .entry(point)
            .and_modify(|score| {
                if new_score < *score {
                    *score = new_score;
                }
            })
            .or_insert(new_score);
    }
}

fn get_new_moves(point_dirs: &HashMap<Point, HashSet<Point>>) -> Vec<Move> {
    let mut moves = Vec::new();
    for (&point, dirs) in point_dirs {
        if dirs.contains(&LEFT) || dirs.contains(&RIGHT) {
            moves.push(Move {
                point,
                dir: UP,
                cost: 1000,
            });
            moves.push(Move {
                point,
                dir: DOWN,
                cost: 1000,
            });
        }
        if dirs.contains(&UP) || dirs.contains(&DOWN) {
            moves.push(Move {
                point,
                dir: LEFT,
                cost: 1000,
            });
            moves.push(Move {
                point,
                dir: RIGHT,
                cost: 1000,
            });
        }
    }
    moves
}

fn solve(map: &Map) -> HashMap<Point, i32> {
    let start = get_pos(&map, 'S');
    let mut point_scores = HashMap::new();
    point_scores.insert(start, 0);

    let first_moves = vec![
        Move {
            point: start,
            dir: RIGHT,
            cost: 0,
        },
        Move {
            point: start,
            dir: UP,
            cost: 1000,
        },
        Move {
            point: start,
            dir: LEFT,
            cost: 2000,
        },
        Move {
            point: start,
            dir: DOWN,
            cost: 1000,
        },
    ];
    traverse(&map, &mut point_scores, &first_moves);
    point_scores
}

pub fn task1() {
    let map = parse_map("../data/t16-2.txt");
    let end = get_pos(&map, 'E');
    let point_scores = solve(&map);
    println!("{:?}", draw2(&map, &point_scores));
    println!("Answer is {:?}", point_scores.get(&end).unwrap());
}
