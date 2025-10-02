use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::{HashMap};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Point(i32, i32);

const CODES: [&str; 5] = ["593A", "508A", "386A", "459A", "246A"];

static DIGIT_TO_POINT: Lazy<HashMap<char, Point>> = Lazy::new(|| {
    HashMap::from([
        ('7', Point(0, 0)),
        ('8', Point(1, 0)),
        ('9', Point(2, 0)),
        ('4', Point(0, 1)),
        ('5', Point(1, 1)),
        ('6', Point(2, 1)),
        ('1', Point(0, 2)),
        ('2', Point(1, 2)),
        ('3', Point(2, 2)),
        ('0', Point(1, 3)),
        ('A', Point(2, 3)),
    ])
});

static POINT_TO_DIGIT: Lazy<HashMap<Point, char>> = Lazy::new(|| {
    DIGIT_TO_POINT
        .iter()
        .map(|(k, v)| (v.clone(), *k))
        .collect()
});

static DIR_TO_POINT: Lazy<HashMap<char, Point>> = Lazy::new(|| {
    HashMap::from([
        ('^', Point(1, 0)),
        ('A', Point(2, 0)),
        ('<', Point(0, 1)),
        ('v', Point(1, 1)),
        ('>', Point(2, 1)),
    ])
});

static POINT_TO_DIR: Lazy<HashMap<Point, char>> =
    Lazy::new(|| DIR_TO_POINT.iter().map(|(k, v)| (v.clone(), *k)).collect());
/*
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+ */

pub fn task1() {
    // let v = vec!["379A"];
    let mut cache: HashMap<(char, char), Vec<String>> = HashMap::new();
    let mut cache_dir: HashMap<(char, char), Vec<String>> = HashMap::new();

    let v = vec!["029A", "980A", "179A", "456A", "379A"];
    let answer = CODES
        .iter()
        .map(|code| {
            let seqs = gen_sequences(code, &DIGIT_TO_POINT, &POINT_TO_DIGIT, &mut cache);
            let seqs2 = seqs.iter().flat_map(|seq| {
                gen_sequences(&seq, &DIR_TO_POINT, &POINT_TO_DIR, &mut cache_dir)
            }).collect::<Vec<_>>();
            let seqs3 = seqs2.iter().flat_map(|seq| {
                gen_sequences(&seq, &DIR_TO_POINT, &POINT_TO_DIR, &mut cache_dir)
            }).collect::<Vec<_>>();
            let min_len = seqs3.iter().map(|s| s.len()).min().unwrap_or(0);
            get_numeric_part(code) * (min_len as i32)
        })
        .sum::<i32>();
    println!("Answer is {:?}", answer);
}

/*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
*/
fn gen_sequences(
    code: &str,
    to_point: &HashMap<char, Point>,
    to_char: &HashMap<Point, char>,
    cache: &mut HashMap<(char, char), Vec<String>>,
) -> Vec<String> {
    let mut seqs = vec![String::new()];
    let mut current = 'A';
    for ch in code.chars() {
        let new_seqs = walk(current, ch, to_point, to_char, cache);
        seqs = product(&seqs, new_seqs);
        current = ch;
    }
    seqs
}

fn walk<'a>(
    a: char,
    b: char,
    to_point: &HashMap<char, Point>,
    to_char: &HashMap<Point, char>,
    cache: &'a mut HashMap<(char, char), Vec<String>>,
) -> &'a [String] {
    let pair = (a, b);
    if cache.contains_key(&pair) {
        return &cache[&pair];
    }
    if a == b {
        cache.insert(pair, vec![String::new()]);
        return cache.get(&pair).unwrap();
    }

    let Point(x1, y1) = to_point[&a];
    let Point(x2, y2) = to_point[&b];
    let dx = signum(x2 - x1);
    let dy = signum(y2 - y1);
    let p1 = Point(x1 + dx, y1);
    let p2 = Point(x1, y1 + dy);

    let mut new_routes = vec![];
    if dx != 0 && to_char.contains_key(&p1) {
        let ch = if dx == 1 { '>' } else { '<' };
        let routes = walk(to_char[&p1], b, to_point, to_char, cache);
        new_routes.extend(prepend(ch, routes));
    }
    if dy != 0 && to_char.contains_key(&p2) {
        let ch = if dy == 1 { 'v' } else { '^' };
        let routes = walk(to_char[&p2], b, to_point, to_char, cache);
        new_routes.extend(prepend(ch, routes));
    }

    cache.insert(pair, new_routes);
    cache.get(&pair).unwrap()
}

fn prepend(ch: char, routes: &[String]) -> Vec<String> {
    routes.iter().map(|s| format!("{ch}{s}")).collect()
}

fn product(left: &[String], right: &[String]) -> Vec<String> {
    let mut p = vec![];
    for a in left {
        for b in right {
            p.push(format!("{a}{b}A"));
        }
    }
    p
}

fn signum(i: i32) -> i32 {
    if i == 0 {
        0
    } else if i > 0 {
        1
    } else {
        -1
    }
}

const RE_NUMBER: Lazy<Regex> = Lazy::new(|| Regex::new(r"(\d+)").unwrap());

fn get_numeric_part(text: &str) -> i32 {
    let caps = RE_NUMBER.captures(text).unwrap();
    //print!("Capt {:?}", caps);
    caps[1].to_string().parse::<i32>().unwrap()
}
