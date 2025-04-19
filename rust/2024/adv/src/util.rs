use std::fs;
use std::hash::Hash;
use std::collections::HashMap;

pub fn read_lines(file_name: &str) -> Vec<String> {
    let content = fs::read_to_string(file_name)
        .expect("Should have been able to read the file");
    content.to_string()     
        .lines()
        .map(str::to_string)
        .collect()
}

pub fn read_all(file_name: &str) -> String {
    let content = fs::read_to_string(file_name)
        .expect("Should have been able to read the file");
    content.to_string()
}

pub fn frequencies<T: Eq + Hash + Clone>(v: &Vec<T>) -> HashMap<T, i64> {
    let mut map = HashMap::new();
    for elem in v {
        *map.entry(elem.clone()).or_insert(0) += 1;
    }
    map
}