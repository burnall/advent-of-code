use std::{collections::{HashMap, HashSet}};
use std::hash::{Hash, Hasher};
use crate::util;

#[derive(Debug)]
struct Data {
    patterns: Vec<String>,
    designs: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    is_value: bool,
    children: HashMap<char, Box<Node>>,
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.is_value.hash(state);
        // HashMap doesn't guarantee order, so we must hash in a consistent way
        let mut sorted: Vec<_> = self.children.iter().collect();
        sorted.sort_by_key(|&(k, _)| k);
        for (k, v) in sorted {
            k.hash(state);
            v.hash(state);
        }
    }
}

pub fn task1() {
    let lines = util::read_lines("../data/t19.txt");
    let data = Data {
        patterns: lines[0].split(", ").map(String::from).collect(),
        designs: lines[2..].to_vec(),
    };
    let root = build_trie(&data.patterns);
    // println!("{:?}", is_matched(&root, "ruwrrbbrgrbruw"));
    let count = data
        .designs
        .iter()
        .filter(|design| is_matched(&root, design))
        .count();
    println!("Count is {:?}", count);
}

fn build_trie(patterns: &[String]) -> Node {
    let mut root = Node {
        is_value: false,
        children: HashMap::new(),
    };
    for pattern in patterns {
        let mut current = &mut root;
        for ch in pattern.chars() {
            current = current.children.entry(ch).or_insert(Box::new(Node {
                is_value: false,
                children: HashMap::new(),
            }));
        }
        current.is_value = true;
    }
    root
}

fn is_matched(trie: &Node, s: &str) -> bool {
    println!("Matching {:?}", s);
    let mut nodes = HashSet::from([trie]);
    for ch in s.chars() {
        let mut new_nodes = HashSet::new();
        for current in nodes {
            if let Some(child) = current.children.get(&ch) {
                new_nodes.insert(&**child);
                if child.is_value {
                    new_nodes.insert(trie);
                }
            }
        }
        if new_nodes.is_empty() {
            return false;
        }
        nodes = new_nodes;
        //println!("Nodes len {:?}", nodes.len());
    }
    nodes.iter().any(|node| node.is_value)
}