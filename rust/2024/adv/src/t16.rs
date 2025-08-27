use crate::util;
use std::collections::HashMap;
use once_cell::sync::Lazy;

type Map = Vec<Vec<char>>;
type Point = (i32, i32);

#[derive(Debug)]
struct Move {
    point: Point, 
    dir: Point
}

const LEFT: Point = (-1, 0);
const UP: Point = (0, -1);
const RIGHT: Point = (1, 0);
const DOWN: Point = (0, 1);
const DIRS: [Point; 4] = [LEFT, UP, RIGHT, DOWN];

const PERPENDICULARS: Lazy<HashMap<Point, Vec<Point>>> = Lazy::new(|| 
    [(LEFT, vec![UP, DOWN]), (RIGHT, vec![UP, DOWN]), 
    (UP, vec![LEFT, RIGHT]), (DOWN, vec![LEFT, RIGHT])].into_iter().collect());

fn parse_map(file_name: &str) -> Map {
    util::read_lines(file_name).iter()
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
    println!("Map: {:?}", scores);
    for y in 0..map.len() {
        let text = (0..map[y].len())
            .map(|x| if map[y][x] == '#' { "#".to_string()} else {scores.get(&(x as i32, y as i32)).unwrap_or(&0).to_string()})
            .collect::<Vec<_>>()
            .join(",");
        println!("{:?}", text);
    }   
}

fn traverse(map: &Map, scores: &mut HashMap<Point, i32>, moves: &Vec<Move>) {
    let mut new_moves = vec![];
    let mut new_scores = HashMap::new();
    let mut first_iter = true;
    for mv in moves {
        let new_scores_update = get_line_scores(map, mv, scores, first_iter);
        println!("New scores update: {:?}", new_scores_update);
        merge_scores(&mut new_scores, &new_scores_update);
        let mut moves_update = get_new_moves(&new_scores_update, &mv.dir);
        new_moves.append(&mut moves_update);
        first_iter = false;
        //draw2(map, scores);
        // break;
    }
    println!("New scores: {:?}", new_scores);
    scores.extend(new_scores.into_iter());
    println!("New scores 2: {:?}", scores);
    println!("New moves: {:?}", new_moves);
    if new_moves.is_empty() {
        return;
    }
    traverse(map, scores, &new_moves);
}

fn get_line_scores(map: &Map, mv: &Move, scores: &HashMap<Point, i32>, turn: bool) -> HashMap<Point, i32> {
    let base_score = scores.get(&mv.point).unwrap() + if turn {1000} else {0};
    let mut line_scores = HashMap::new();
    let mut i = 0;
    let mut next_point = mv.point;
    println!("get_line_scores, mv {:?}", mv);
    println!("get_line_scores, scores {:?}", scores);
    loop {
        next_point = (next_point.0 + mv.dir.0, next_point.1 + mv.dir.1);
        println!("get_line_scores, next_point: {:?}", next_point);
        i += 1;
        if scores.contains_key(&next_point) {
            continue;
        }
        if map[next_point.1 as usize][next_point.0 as usize] == '#' {
            return line_scores;
        }
        line_scores.insert(next_point, base_score + i);
    }
} 

fn get_new_moves(scores: &HashMap<Point, i32>, dir: &Point) -> Vec<Move> {
    vec![]
} 

fn merge_scores(scores: &mut HashMap<Point, i32>, scores_update: &HashMap<Point, i32>) {
    for (&point, &new_score) in scores_update {
        scores.entry(point)
            .and_modify(|score| if new_score < *score {*score = new_score;})
            .or_insert(new_score);
    }
}

pub fn task1() {
    let map = parse_map("../data/t16-2.txt");
    let start = get_pos(&map, 'S');
    let end = get_pos(&map, 'E');
    let mut scores = HashMap::new();
    scores.insert(start, 0);
    let first_moves = DIRS.into_iter().map(|dir| Move{point: start, dir: dir}).collect();
    traverse(&map, &mut scores, &first_moves);

    println!("First moves: {:?}", first_moves);
    println!("{:?}", draw2(&map, &scores));
}