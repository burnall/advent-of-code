use crate::util;

fn parse_input(file_name: &str) -> Vec<i8> {
    let v = util::read_lines(file_name)[0]
        .chars()
        .map(|ch| ch.to_digit(10).unwrap() as i8)
        .collect();
    v
}

fn interpret(disk_map: &Vec<i8>) -> Vec<i32> {
    let mut disk = Vec::with_capacity(disk_map.len() * 10);
    let mut id = 0;
    for i in 0..disk_map.len() {
        let block = if i % 2 == 0 { id += 1; id - 1} else { -1 };
        for j in 0..(disk_map[i] as i32) {
            disk.push(block);  
        }   
    }
    disk
}

fn compact(disk: &mut Vec<i32>) {
   let mut len = disk.len();
   for i in 0..len {
      if disk[i] != -1 {
           continue;
      }
      for j in (0..len).rev() {
          if disk[j] != -1 {
              break;
          }  
          len -= 1;
      }
      if i >= len {
          break;
      }
      disk[i] = disk[len - 1];
      len -= 1;
   }
   disk.resize(len, 0);
}

fn crc(disk: &Vec<i32>) -> i64 {
    let mut crc = 0;
    for i in 0..disk.len() {
        crc += (disk[i] as i64) * (i as i64);
    }
    crc
}

pub fn task1() {
   let disk_map = parse_input("../data/t09.txt");
   //let disk_map = vec![1, 2, 3, 4, 5, 6];
   // let disk_map = vec![2,3,3,3,1,3,3,1,2,1,4,1,4,1,3,1,4,0,2];
   let mut disk = interpret(&disk_map);
   compact(&mut disk);
   println!("{:?}", crc(&disk));
}   
