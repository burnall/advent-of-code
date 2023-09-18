use std::fs;

pub fn read_lines(file_name: &str) -> Vec<String> {
    let content = fs::read_to_string(file_name)
        .expect("Should have been able to read the file");
    content.to_string()     
        .lines()
        .map(str::to_string)
        .collect()
}