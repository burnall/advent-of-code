use crate::util::read_lines;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum DevType {
    Key,
    Lock,
}

#[derive(Debug)]
struct Device {
    dev_type: DevType,
    pins: [u8; 5],
}

pub fn task1() {
    let devices = read_devices();
    let mut count = 0;
    for key in devices.iter().filter(|d| d.dev_type == DevType::Key) {
        for lock in devices.iter().filter(|d| d.dev_type == DevType::Lock) {
            if is_fit_together(key, lock) {
                count += 1;
            }
        }
    }
    println!("Answer: {:?}", count);
}

fn read_devices() -> Vec<Device> {
    let mut lines = read_lines("../data/t25.txt");
    lines.push(String::new());
    let mut devices = Vec::new();
    let mut raw = Vec::new();
    for line in lines {
        if line.is_empty() {
            devices.push(read_device(raw));
            raw = Vec::new();
        } else {
            raw.push(line);
        }
    }
    devices
}

/*
#####
.####
.####
.####
.#.#.
.#...
.....
*/

fn read_device(raw: Vec<String>) -> Device {
    if raw.len() != 7 || raw.iter().any(|line| line.len() != 5) {
        panic!("Invalid device: {:?}", raw);
    }
    let dev_type = if raw[0] == "#####" {
        DevType::Lock
    } else {
        DevType::Key
    };
    let mut pins = [0_u8; 5];
    for col in 0..5 {
        for (idx, row) in make_range(dev_type == DevType::Lock).enumerate() {
            if raw[row].chars().nth(col) == Some('.') {
                pins[col] = idx as u8;
                break;
            }
        }
    }
    Device { dev_type, pins }
}

fn make_range(forward: bool) -> Box<dyn Iterator<Item = usize>> {
    if forward {
        Box::new(1..=6)
    } else {
        Box::new((0..=5).rev())
    }
}

fn is_fit_together(key: &Device, lock: &Device) -> bool {
    (0..5).all(|i| key.pins[i] + lock.pins[i] <= 5)
}