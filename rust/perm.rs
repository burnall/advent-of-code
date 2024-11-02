fn perm(a: &mut [i32], n: usize) {
    if n == 1 {
        println!("{:?}", a); 
        return;
    }
    perm(a, n - 1);
    
    for i in 0..(n - 1) {
        let j = if n % 2 == 0 {i} else {0}; 
        let temp = a[j];
        a[j] = a[n - 1];
        a[n - 1] = temp;
        
        perm(a, n - 1);
    }
}

fn main() {
    let mut a = [1, 2, 3, 4]; 
    perm(&mut a, 4);
}
