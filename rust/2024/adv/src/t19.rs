use crate::util;
use std::collections::{HashMap, HashSet};
use std::time::Instant;

#[derive(Debug)]
struct Data {
    patterns: Vec<String>,
    designs: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    is_value: bool,
    children: HashMap<char, Node>,
}

impl Default for Node {
    fn default() -> Self {
        Node {
            is_value: false,
            children: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct State {
    id: usize,
    transitions: HashMap<char, usize>,
    is_terminal: bool,
}

#[derive(Debug)]
struct StateMachine {
    states: HashMap<usize, State>,
    start_state: usize,
}

pub fn task1() {
    let lines = util::read_lines("../data/t19.txt");
    let data = Data {
        patterns: lines[0].split(", ").map(String::from).collect(),
        designs: lines[2..].to_vec(),
    };
    let root = build_trie(&data.patterns);
    let machine = trie_to_states(&root);
    //println!("States {:#?}", machine);

    let start = Instant::now();
    // r, wr, b, g, bwu, rb, gb, br
    // println!("{:?}", is_matched(&machine, "bwurbbwurb"));
    let count = data
        .designs
        .iter()
        .filter(|design| is_matched(&machine, design))
        .count();
    let duration = start.elapsed();

    println!("Count is {:?} in {:?}", count, duration);
}

fn build_trie(patterns: &[String]) -> Node {
    let mut root = Node {
        is_value: false,
        children: HashMap::new(),
    };
    for pattern in patterns {
        let mut current = &mut root;
        for ch in pattern.chars() {
            current = current
                .children
                .entry(ch)
                .or_insert(Node::default());
        }   
        current.is_value = true;
    }
    root
}

fn trie_to_states(trie: &Node) -> StateMachine {
    let final_state = State {
        id: 0,
        transitions: HashMap::new(),
        is_terminal: true,
    };
    let mut states = HashMap::from([(0, final_state)]);
    StateMachine { 
        start_state: process_node(trie, &mut states, &mut 1),
        states
    }
}

fn process_node(node: &Node, states: &mut HashMap<usize, State>, next_id: &mut usize) -> usize {
    if node.children.is_empty() {
        return 0;
    }

    let id = *next_id;
    *next_id += 1;  
    let mut transitions = HashMap::new();
    for (ch, child) in &node.children {
        let child_id = process_node(child, states, next_id);
        transitions.insert(*ch, child_id);
    }
    let state = State {
        id,
        transitions,
        is_terminal: node.is_value,
    };
    states.insert(id, state);
    id
}

fn is_matched(machine: &StateMachine, s: &str) -> bool {
    let StateMachine {start_state, states} = machine;
    let mut current_states = HashSet::from([*start_state]);
    for ch in s.chars() {
        let mut new_states = HashSet::new();
        for state_id in &current_states {
            let state = states.get(state_id).unwrap();
            if let Some(next_state_id) = state.transitions.get(&ch) {
                new_states.insert(*next_state_id);
                let next_state = states.get(next_state_id).unwrap();
                if next_state.is_terminal {
                    new_states.insert(*start_state);
                }
            }
        }
        if new_states.is_empty() {
            return false;
        }
        current_states = new_states;
    }
    current_states.iter().any(|state_id| machine.states.get(state_id).unwrap().is_terminal)
}

