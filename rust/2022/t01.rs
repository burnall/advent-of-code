mod util;

#[derive(Debug)]
struct Info {
    groups: Vec<Vec<i32>>, 
    current_group: Vec<i32>
}

fn fold_lines(mut agg: Info, s: String) -> Info {
    if s == "" {
        agg.groups.push(agg.current_group);
        agg.current_group = Vec::new();
    } else {
        agg.current_group.push(s.parse().unwrap());
    }
    agg    
}

fn main() {
    let mut lines = util::read_lines("../data/t01.txt");
    lines.push(String::new());    
    let groups: Vec<i32> = lines
        .into_iter()
        .fold(Info {groups: Vec::new(), current_group: Vec::new()}, fold_lines)
        .groups
        .iter()
        .map(|v| v.into_iter().sum::<i32>())
        .collect();

    let sum = groups.clone()
        .into_iter()
        .max()
        .unwrap();
    println!("Part 1: {:?}", sum);

    let mut groups2 = groups.to_vec();
    groups2.sort_by(|a, b| b.cmp(a));
    let sum3_biggest = &groups2[0..3].into_iter().sum::<i32>();
    println!("Part 1: {:?}", sum3_biggest);
}