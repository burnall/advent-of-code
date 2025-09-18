use crate::util;
use std::collections::HashMap;
use std::collections::HashSet;

type Map = Vec<Vec<char>>;
type Point = (i32, i32);
type PointCost = (Point, i32);

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
enum Direction {
    Left,
    Up,
    Right,
    Down,
}

type Cost = HashMap<Direction, i32>;
const DIRS: [Direction; 4] = [
    Direction::Left,
    Direction::Up,
    Direction::Right,
    Direction::Down,
];

fn get_perp(cost: &Cost, dir: Direction) -> Option<i32> {
    match dir {
        Direction::Left | Direction::Right => {
            min_opt(cost.get(&Direction::Up), cost.get(&Direction::Down))
        }
        _ => min_opt(cost.get(&Direction::Left), cost.get(&Direction::Right)),
    }
}

fn min_opt(a: Option<&i32>, b: Option<&i32>) -> Option<i32> {
    match (a, b) {
        (None, None) => None,
        (Some(&x), None) => Some(x),
        (None, Some(&y)) => Some(y),
        (Some(&x), Some(&y)) => Some(x.min(y)),
    }
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
                return (x as i32, y as i32);
            }
        }
    }
    panic!("{} not found", ch);
}

fn get_nb(point: &Point, dir: Direction) -> Point {
    match dir {
        Direction::Left => (point.0 - 1, point.1),
        Direction::Up => (point.0, point.1 - 1),
        Direction::Right => (point.0 + 1, point.1),
        Direction::Down => (point.0, point.1 + 1),
    }
}

fn draw(map: &Map, costs: &HashMap<Point, Cost>) {
    let empty_map = HashMap::new();
    for y in 0..map.len() {
        let text = (0..map[y].len())
            .map(|x| {
                if map[y][x] == '#' {
                    "#".to_string()
                } else {
                    let v = costs
                        .get(&(x as i32, y as i32))
                        .unwrap_or(&empty_map)
                        .values()
                        .min();
                    v.unwrap_or(&-1).to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(",");
        println!("{:?}", text);
    }
}

fn update_score(cost: &Cost, cost_nb: &mut Cost, dir: Direction) -> bool {
    let mut changed = false;
    if cost.contains_key(&dir) {
        let new_score = cost.get(&dir).unwrap() + 1;
        if !cost_nb.contains_key(&dir) || *cost_nb.get(&dir).unwrap() > new_score {
            cost_nb.insert(dir, new_score);
            changed = true;
        }
    }
    match get_perp(cost, dir) {
        Some(new_score) => {
            if !cost_nb.contains_key(&dir) || *cost_nb.get(&dir).unwrap() > new_score + 1001 {
                cost_nb.insert(dir, new_score + 1001);
                changed = true;
            }
        }
        None => {}
    }
    return changed;
}

fn traverse(map: &Map, costs: &mut HashMap<Point, Cost>, points: &[Point]) {
    let mut next_points = vec![];
    for point in points {
        let cost = costs.get(point).unwrap().clone();
        for dir in DIRS {
            let nb = get_nb(point, dir);
            if map[nb.1 as usize][nb.0 as usize] == '#' {
                continue;
            }
            costs.entry(nb).or_insert(HashMap::new());
            let cost_nb = costs.get_mut(&nb).unwrap();
            if update_score(&cost, cost_nb, dir) {
                next_points.push(nb);
            }
        }
    }
    if !next_points.is_empty() {
        traverse(map, costs, &next_points);
    }
}

fn solve(map: &Map) -> HashMap<Point, Cost> {
    let start = get_pos(&map, 'S');
    let cost = HashMap::from([(Direction::Left, 0)]);
    let mut costs = HashMap::from([(start, cost)]);
    traverse(map, &mut costs, &vec![start]);
    costs
}

pub fn task1() {
    let map = parse_map("../data/t16.txt");
    let end = get_pos(&map, 'E');
    let costs = solve(&map);
    println!("{:?}", draw(&map, &costs));
    println!("Answer is {:?}", costs.get(&end));
}

fn best_path(costs: &HashMap<Point, Cost>, path: &mut HashSet<Point>, active_points: &HashSet<PointCost>) {
    let mut next_points = HashSet::new();
    for (point, score) in active_points {
        for dir in DIRS {
            let nb = get_nb(point, dir);
            if !costs.contains_key(&nb) {
                continue;
            }
            let cost_nb = costs.get(&nb).unwrap();
            cost_nb.values()
                .filter(|&cost| cost + 1 == *score || cost + 1001 == *score)
                .for_each(|&cost| {
                    path.insert(nb);
                    next_points.insert((nb, cost));
                });
        }
    }

    if !active_points.is_empty() {
        best_path(costs, path, &next_points);
    }
}

pub fn task2() {
    let map = parse_map("../data/t16.txt");
    let end = get_pos(&map, 'E');
    let costs = solve(&map);
    let mut path = HashSet::from([end]);

    let end_cost = costs.get(&end).unwrap();
    let min = end_cost.values().min().unwrap();
    let point_costs = HashSet::from([(end, *min)]);
    best_path(&costs, &mut path, &point_costs);
    println!("Answer is {:?}", path.len());
}
