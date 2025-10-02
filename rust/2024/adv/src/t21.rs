use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;

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

static DIR_TO_POINT: Lazy<HashMap<char, Point>> = Lazy::new(|| {
    HashMap::from([
        ('^', Point(1, 0)),
        ('A', Point(2, 0)),
        ('<', Point(0, 1)),
        ('v', Point(1, 1)),
        ('>', Point(2, 1)),
    ])
});
/*
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+ */

pub fn task1() {
    // let sequences: Vec<String> = CODES.iter().map(|code| gen_sequence(code)).collect();
    let v = vec!["379A"]; 
    // let v = vec!["029A", "980A", "179A", "456A", "379A"]; 
    let answer = v
        .iter()
        .map(|code| {
            let seq = gen_sequence(code, &DIGIT_TO_POINT);
            println!("{}", seq);
            let seq2 = gen_sequence(&seq, &DIR_TO_POINT);
            let seq3 = gen_sequence(&seq2, &DIR_TO_POINT);
            println!("Hmm {} {}", get_numeric_part(code), seq3.len());
            get_numeric_part(code) * (seq3.len() as i32)
        })
        .sum::<i32>();
    // let seq = gen_sequence("029A", &DIGIT_TO_POINT);
    println!("Answer is {:?}", answer);
}

// <A^A>^^AvvvA
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
fn gen_sequence(code: &str, mapping: &HashMap<char, Point>) -> String {
    let mut sequence = String::new();
    let mut current = 'A';
    for ch in code.chars() {
        sequence.push_str(&walk(current, ch, mapping));
        sequence.push('A');
        current = ch;
    }
    sequence
}

fn walk(a: char, b: char, mapping: &HashMap<char, Point>) -> String {
    let mut seq;
    if mapping[&a].0 == 0 {
        seq = walk_row(a, b, mapping);
        seq.push_str(&walk_col(a, b, mapping));
    } else {
        seq = walk_col(a, b, mapping);
        seq.push_str(&walk_row(a, b, mapping));
    }
    seq
}

fn walk_row(a: char, b: char, mapping: &HashMap<char, Point>) -> String {
    let Point(x1, _) = mapping[&a];
    let Point(x2, _) = mapping[&b];
    let mut seq = String::new();

    for _ in 0..(x1 - x2).abs() {
        seq.push(if x1 < x2 { '>' } else { '<' });
    }
    seq
}

fn walk_col(a: char, b: char, mapping: &HashMap<char, Point>) -> String {
    let Point(_, y1) = mapping[&a];
    let Point(_, y2) = mapping[&b];
    let mut seq = String::new();

    for _ in 0..(y1 - y2).abs() {
        seq.push(if y1 < y2 { 'v' } else { '^' });
    }
    seq
}

const RE_NUMBER: Lazy<Regex> = Lazy::new(|| Regex::new(r"(\d+)").unwrap());

fn get_numeric_part(text: &str) -> i32 {
    let caps = RE_NUMBER.captures(text).unwrap();
    //print!("Capt {:?}", caps);
    caps[1].to_string().parse::<i32>().unwrap()
}
