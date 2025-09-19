use once_cell::sync::Lazy;

#[derive(Clone, Debug)]
struct Program {
    a: i64,
    b: i64,
    c: i64,
    steps: &'static [i64],
    ptr: usize,
    output: Vec<i64>,
}

fn clear(program: &mut Program) {
    program.a = 0;
    program.b = 0;
    program.c = 0;
    program.ptr = 0;
    program.output.clear();
}

static P1: Lazy<Program> = Lazy::new(|| Program {
    a: 729,
    b: 0,
    c: 0,
    steps: &[0, 1, 5, 4, 3, 0],
    ptr: 0,
    output: vec![],
});

static P0: Program = Program {
    a: 64584136,
    b: 0,
    c: 0,
    steps: &[2, 4, 1, 2, 7, 5, 1, 3, 4, 3, 5, 5, 0, 3, 3, 0],
    // steps: &[0,3,5,4,3,0],
    ptr: 0,
    output: vec![],
};

fn stopped(p: &Program) -> bool {
    p.ptr as usize >= p.steps.len()
}

fn operand(p: &Program, op: i64) -> i64 {
    match op {
        0 | 1 | 2 | 3 => op,
        4 => p.a,
        5 => p.b,
        6 => p.c,
        7 => -1,
        _ => panic!("bad op"),
    }
}

/*
[
2, 4,  B = A % 8
1, 2,  B = B ^ 2
7, 5,  C = A / 2^B
1, 3,  B = B ^ 3
4, 3,  B = B ^ C
5, 5,  out B % 8
0, 3,  A = A / 8
3, 0   jump zero if !A
],
 */
fn step(program: &mut Program) -> bool {
    let operation = program.steps[program.ptr];
    let literal_param = program.steps[program.ptr + 1];
    let param = operand(program, literal_param);
    let mut next_ptr = program.ptr + 2;
    match operation {
        0 => {
            program.a /= 2_i64.pow(param as u32);
        }
        1 => {
            program.b ^= literal_param;
        }
        2 => {
            program.b = param % 8;
        }
        3 => {
            if program.a != 0 {
                next_ptr = param as usize;
            }
        }
        4 => {
            program.b ^= program.c;
        }
        5 => {
            program.output.push(param % 8);
        }
        6 => {
            program.b = program.a / 2_i64.pow(param as u32);
        }
        7 => {
            program.c = program.a / 2_i64.pow(param as u32);
        }
        _ => panic!("bad op"),
    }

    program.ptr = next_ptr;
    true
}

fn run(program: &mut Program, max_steps: i64) {
    let mut step_count = max_steps;
    while !stopped(&program) {
        if step_count == 0 {
            panic!("Too many steps for program: {:?}", program);
        }
        step(program);
        step_count -= 1;
    }
}

pub fn task1() {
    let mut program = P0.clone();
    run(&mut program, 100000);
    println!("{:?}", program);
    let result = program
        .output
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    println!("{:?}", result);
}

pub fn task2() {
    let mut program = P0.clone();
    let mut steps = P0.steps.to_vec();
    steps.reverse();
    let mut final_a_s = vec![0];

    for idx in 0..steps.len() {
        let mut final_a_s_new = vec![];
        for final_a in final_a_s {
            for a in 0..8 {
                clear(&mut program);
                program.a = final_a * 8 + a;
                run(&mut program, 10000);
                if P0.steps.ends_with(&program.output) {
                    final_a_s_new.push(final_a * 8 + a);
                }
            }
        }
        if final_a_s_new.is_empty() {
            panic!("No solution found for step {:?}", idx);
        }
        final_a_s = final_a_s_new;
    }
    println!("{:?}", final_a_s.iter().min().unwrap());
}
