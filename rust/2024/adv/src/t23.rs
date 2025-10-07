use crate::util::read_lines;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct Graph {
    edges: HashMap<usize, HashSet<usize>>,
    names: HashMap<usize, String>,
    all_edges: HashSet<(usize, usize)>,
}

pub fn task1() {
    let graph = get_graph();
    let cnt = find_triangles(&graph)
        .iter()
        .filter(|vs| vs.iter().any(|id| graph.names[id].starts_with('t')))
        .count();
    println!("Count: {}", cnt);
}

pub fn task2() {
    let graph = get_graph();
    let mut current = find_triangles(&graph);
    loop {
        current = extend_loops(&graph, &current);
        println!("Count: {}", current.len());
        if current.len() == 1 {
            break;
        }
        if current.is_empty() {
            panic!("No loops found");
        }
    }

    let mut names: Vec<String> = current
        .iter()
        .next()
        .unwrap()
        .iter()
        .map(|id| graph.names[id].clone())
        .collect();
    names.sort();
    println!("Answer: {:?}", names.join(","));
}

fn extend_loops(graph: &Graph, loops: &HashSet<Vec<usize>>) -> HashSet<Vec<usize>> {
    let mut extended = HashSet::new();
    for aloop in loops {
        for neighbor in &graph.edges[&aloop[0]] {
            if aloop
                .iter()
                .all(|v| graph.all_edges.contains(&(*neighbor, *v)))
            {
                let mut new_loop = aloop.clone();
                new_loop.push(*neighbor);
                new_loop.sort();
                extended.insert(new_loop);
            }
        }
    }
    extended
}

fn get_graph() -> Graph {
    let pairs: Vec<(String, String)> = read_lines("../data/t23.txt")
        .iter()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            (a.to_string(), b.to_string())
        })
        .collect();
    build_graph(&pairs)
}

fn build_graph(pairs: &Vec<(String, String)>) -> Graph {
    let mut edges: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut names: HashMap<usize, String> = HashMap::new();
    let mut ids: HashMap<String, usize> = HashMap::new();
    let mut all_edges: HashSet<(usize, usize)> = HashSet::new();
    let mut next_id: usize = 0;

    for (a, b) in pairs {
        let a_id = *ids.entry(a.clone()).or_insert_with(|| {
            let id = next_id;
            names.insert(next_id, a.clone());
            next_id += 1;
            id
        });
        let b_id = *ids.entry(b.clone()).or_insert_with(|| {
            let id = next_id;
            names.insert(id, b.clone());
            next_id += 1;
            id
        });
        edges.entry(a_id).or_insert_with(HashSet::new).insert(b_id);
        edges.entry(b_id).or_insert_with(HashSet::new).insert(a_id);
        all_edges.insert((a_id, b_id));
        all_edges.insert((b_id, a_id));
    }

    Graph {
        edges,
        names,
        all_edges,
    }
}

fn find_triangles(graph: &Graph) -> HashSet<Vec<usize>> {
    let mut triangles = HashSet::new();
    for (a, b) in &graph.all_edges {
        for c in &graph.edges[a] {
            if graph.all_edges.contains(&(*b, *c)) {
                let mut triangle = vec![*a, *b, *c];
                triangle.sort();
                triangles.insert(triangle);
            }
        }
    }
    triangles
}
