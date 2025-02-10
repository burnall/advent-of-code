use crate::util;

#[derive(Debug)]
struct Manual {
    rules: Vec<(i32, i32)>,
    updates: Vec<Vec<i32>>
}

fn parse_rules(lines: &[String]) -> Vec<(i32, i32)> {
    lines.iter()
        .map(|line| {
            let parts: Vec<_> = line.split("|")
                .map(|s| s.parse::<i32>().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .collect()    
}

fn parse_updates(lines: &[String]) -> Vec<Vec<i32>> {
    lines.iter()
        .map(|line| line.split(",")
                .map(|s| s.parse::<i32>().unwrap())
                .collect())
        .collect()    
}

fn parse_input(file_name: &str) -> Manual {
    let lines = util::read_lines(file_name);
    let index_empty = lines.iter().position(|line| line == "").unwrap();
    
    let rules = parse_rules(&lines[..index_empty]);
    let updates = parse_updates(&lines[(index_empty + 1)..]);

    Manual {rules: rules, updates: updates}
}

fn is_matching_rule(update: &Vec<i32>, rule: &(i32, i32)) -> bool {
    let idx_left = update.iter().position(|i| *i == rule.0);
    let idx_right = update.iter().position(|i| *i == rule.1);
    idx_left == None || idx_right == None || idx_left < idx_right
}

fn get_valid_updates(manual: &Manual) -> Vec<&Vec<i32>> {
    manual.updates.iter()
        .filter(|update| manual.rules.iter().all(|rule| is_matching_rule(update, rule)))
        .collect()
}

fn get_middle(v: &&Vec<i32>) -> i32 {
    if v.len() % 2 == 0 {
        panic!("Vector should have odd length, {:?}", v);
    }
    v[v.len() / 2]
}

pub fn task1() {
   let manual = parse_input("../data/t05.txt");
   let updates = get_valid_updates(&manual);
   let result: i32 = updates.iter().map(get_middle).sum();
   println!("{:?}", result);
}