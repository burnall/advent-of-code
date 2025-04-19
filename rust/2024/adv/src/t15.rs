use crate::util;
use once_cell::sync::Lazy;
use std::collections::HashMap;

type Map = Vec<Vec<char>>;
type Point = (i32, i32);

#[derive(Debug)]
struct Config {
    map: Map,
    route: Vec<char>
}

fn parse_input(file_name: &str) -> Config {
    let text = util::read_all(file_name);
    let pos = text.find("\n\n").unwrap();
    let (map, route) = text.split_at(pos);
    Config {
        map: map.lines()
                .map(|line| line.chars().collect())
                .collect(),
        route: route.replace("\n", "")
                    .chars()
                    .collect()
    }
}

fn tour(map: &mut Map, route: &[char]) {
    let mut current_p = get_start_pos(map);
    for mv in route {
        if let Some(empty_p) = find_empty(map, &current_p, *mv) {
            map[current_p.1 as usize][current_p.0 as usize] = '.';
            let next_p = get_next_point(&current_p, *mv);
            map[next_p.1 as usize][next_p.0 as usize] = '@';
            // println!("Start ({:?}), Next {:?}", start_p, next_p);
            if !is_adjacent(&current_p, &empty_p) {
                map[empty_p.1 as usize][empty_p.0 as usize] = 'O';
            }
            current_p = next_p;
        }
        // draw(&map, *mv);
    }
}

const DIRS: Lazy<HashMap<char, (i32, i32)>> = Lazy::new(|| 
    [('>', (1, 0)), ('v', (0, 1)), ('<', (-1, 0)), ('^', (0, -1))].into_iter().collect());

fn find_empty(map: &Map, pos: &Point, mv: char) -> Option<Point> {
    let &(dx, dy) = DIRS.get(&mv).unwrap();
    let (mut x, mut y) = (pos.0 as i32, pos.1 as i32);
    loop {
        (x, y) = (x as i32 + dx, y as i32 + dy);
        let ch = map[y as usize][x as usize];
        if ch == '#' {
            return None;
        } 
        if ch == '.' {
            return Some((x, y));
        }
    }
}

fn is_adjacent((ax, ay): &Point, (bx, by): &Point) -> bool {
    ((ax - bx) as i32).abs() + ((ay - by) as i32).abs() == 1
}

fn get_next_point(&(x, y): &Point, mv: char) -> Point {
    let &(dx, dy) = DIRS.get(&mv).unwrap();
    (x as i32 + dx, y as i32 + dy)
}

fn get_start_pos(map: &Map) -> Point {
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == '@' {
                return (x as i32, y as i32);
            }
        }
    }
    panic!("@ not found");
}

fn draw(map: &Map, mv: char) {
    println!("Map ({:?}):", mv);
    for line in map {
        let text: String = line.iter().collect();
        println!("{:?}", text);
    }   
}

fn eval(map: &Map) -> usize {
    let mut sum = 0;
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == 'O' {
                sum += 100 * y + x;
            }
        }
    }
    sum
}

pub fn task1() {
    let Config {mut map, route} = parse_input("../data/t15.txt");
    tour(&mut map, &route);
    draw(&map, '-');

    println!("{:?}", eval(&map));
}