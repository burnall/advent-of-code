use once_cell::sync::Lazy;

#[derive(Clone, Debug)]
struct Program {
    a: i32,
    b: i32,
    c: i32,
    steps: &'static [i32],
    ptr: usize,
    output: Vec<i32>,
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
    ptr: 0,
    output: vec![],
};

fn stopped(p: &Program) -> bool {
    p.ptr as usize >= p.steps.len()
}

fn operand(p: &Program, op: i32) -> i32 {
    match op {
        0 | 1 | 2 | 3 => op,
        4 => p.a,
        5 => p.b,
        6 => p.c,
        7 => -1,
        _ => panic!("bad op"),
    }
}

fn step(program: &mut Program) {
    println!("Before {:?}", program);  
    let operation = program.steps[program.ptr];
    let literal_param = program.steps[program.ptr + 1];
    let param = operand(program, literal_param);
    let mut next_ptr = program.ptr + 2;
    match operation {
        0 => {
            program.a /= 2_i32.pow(param as u32);
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
            program.b = program.a / 2_i32.pow(param as u32);
        }
        7 => {
            program.c = program.a / 2_i32.pow(param as u32);
        }
        _ => panic!("bad op"),
    }

    program.ptr = next_ptr;
    println!("After {:?}", program);  
}

fn run(program: &mut Program, max_steps: i32) {
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
    let result = program.output
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    println!("{:?}", result);  
}
