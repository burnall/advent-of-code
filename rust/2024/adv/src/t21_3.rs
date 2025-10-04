use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Point(i32, i32);

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Seq {
    part: String,
    depth: usize,
}

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

/*
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+ */

pub fn task1() {
    // let v = vec!["029A", "980A", "179A", "456A", "379A"];
    let answer = CODES
        .iter()
        .map(|code| {
            let seq = gen_sequence(code);
            println!("Seq {} {}", code, seq);
            let seq2 = gen_sequence_dir(&seq);
            println!("Seq2 {} {}", code, seq2);
            let seq3 = gen_sequence_dir(&seq2);
            println!("Hmm {} {}", get_numeric_part(code), seq3.len());
            get_numeric_part(code) * seq3.len()
        })
        .sum::<usize>();
    println!("Answer is {:?}", answer);
}

pub fn task2() {
    // let v = vec!["029A", "980A", "179A", "456A", "379A"];
    let answer = CODES
        .iter()
        .map(|code| {
            let mut seq = gen_sequence(code);
            println!("Seq {} {}", code, seq);
            for i in 0..10 {
                seq = gen_sequence_dir(&seq);
                println!("Seq {} {}", i, seq.len());
            }
            println!("Hmm {} {}", get_numeric_part(code), seq.len());
            get_numeric_part(code) * seq.len()
        })
        .sum::<usize>();
    println!("Answer is {:?}", answer);
}

pub fn task2_1() {
    let mut cache = HashMap::new();
    // let v = vec!["029A", "980A", "179A", "456A", "379A"];
    let answer = CODES
        .iter()
        .map(|code| {
            let seq = gen_sequence(code);
            let len = get_seq_length(&seq, 25, &mut cache);
            println!("Result {} {}", get_numeric_part(code), len);
            get_numeric_part(code) * len
        })
        .sum::<usize>();
    println!("Cache length {:?}", cache.len());
    println!("Answer is {:?}", answer);
}

fn get_seq_length(seq: &str, depth: usize, cache: &mut HashMap<Seq, usize>) -> usize {
    let parts: Vec<&str> = seq.split_inclusive('A').collect();
    parts
        .iter()
        .map(|part| {
            get_part_length(
                Seq {
                    part: part.to_string(),
                    depth,
                },
                cache,
            )
        })
        .sum()
}

fn get_part_length(seq: Seq, cache: &mut HashMap<Seq, usize>) -> usize {
    if seq.depth == 0 {
        return seq.part.len();
    }
    if let Some(len) = cache.get(&seq) {
        return *len;
    }
    let len = gen_sequence_dir(&seq.part)
        .split_inclusive('A')
        .map(|part| {
            get_part_length(
                Seq {
                    part: part.to_string(),
                    depth: seq.depth - 1,
                },
                cache,
            )
        })
        .sum();
    cache.insert(seq, len);
    len
}

fn gen_sequence(code: &str) -> String {
    let mut sequence = String::new();
    let mut current = 'A';
    for ch in code.chars() {
        sequence.push_str(&walk(current, ch));
        sequence.push('A');
        current = ch;
    }
    sequence
}

fn gen_sequence_dir(code: &str) -> String {
    let mut sequence = String::new();
    let mut current = 'A';
    for ch in code.chars() {
        if current != ch {
            sequence.push_str(PAIR_TO_STRING[&(current, ch)]);
        }

        sequence.push('A');
        current = ch;
    }
    sequence
}

fn walk(a: char, b: char) -> String {
    if a == b {
        return String::new();
    }
    let Point(x1, y1) = DIGIT_TO_POINT[&a];
    let Point(x2, y2) = DIGIT_TO_POINT[&b];
    /*
    If the blank space forces you into a route, do it
    Otherwise, if we need to go left, do leri + updo
    Otherwise, do updo + leri */
    let mut seq;
    if x1 == 0 && y2 == 3 {
        seq = walk_row(a, b);
        seq.push_str(&walk_col(a, b));
    } else if y1 == 3 && x2 == 0 {
        seq = walk_col(a, b);
        seq.push_str(&walk_row(a, b));
    } else if x1 > x2 {
        seq = walk_row(a, b);
        seq.push_str(&walk_col(a, b));
    } else {
        seq = walk_col(a, b);
        seq.push_str(&walk_row(a, b));
    }

    seq
}

fn walk_row(a: char, b: char) -> String {
    let Point(x1, _) = DIGIT_TO_POINT[&a];
    let Point(x2, _) = DIGIT_TO_POINT[&b];
    let mut seq = String::new();

    for _ in 0..(x1 - x2).abs() {
        seq.push(if x1 < x2 { '>' } else { '<' });
    }
    seq
}

fn walk_col(a: char, b: char) -> String {
    let Point(_, y1) = DIGIT_TO_POINT[&a];
    let Point(_, y2) = DIGIT_TO_POINT[&b];
    let mut seq = String::new();

    for _ in 0..(y1 - y2).abs() {
        seq.push(if y1 < y2 { 'v' } else { '^' });
    }
    seq
}

const RE_NUMBER: Lazy<Regex> = Lazy::new(|| Regex::new(r"(\d+)").unwrap());

static PAIR_TO_STRING: Lazy<HashMap<(char, char), &'static str>> = Lazy::new(|| {
    HashMap::from([
        (('<', 'v'), ">"),
        (('<', '>'), ">>"),
        (('<', '^'), ">^"),
        (('<', 'A'), ">>^"),
        (('A', '<'), "v<<"),
        (('^', '<'), "v<"),
        (('>', '<'), "<<"),
        (('v', '<'), "<"),
        (('^', 'A'), ">"),
        (('^', 'v'), "v"),
        (('^', '>'), "v>"),
        (('A', '>'), "v"),
        (('A', '^'), "<"),
        (('A', 'v'), "<v"),
        (('v', '>'), ">"),
        (('v', '^'), "^"),
        (('v', 'A'), "^>"),
        (('>', 'A'), "^"),
        (('>', '^'), "<^"),
        (('>', 'v'), "<"),
    ])
});

fn get_numeric_part(text: &str) -> usize {
    let caps = RE_NUMBER.captures(text).unwrap();
    caps[1].to_string().parse::<usize>().unwrap()
}
