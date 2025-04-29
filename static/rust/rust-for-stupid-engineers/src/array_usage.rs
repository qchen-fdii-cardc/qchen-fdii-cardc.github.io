pub fn eye10() -> [[f64; 10]; 10] {
    let mut array = [[0.0; 10]; 10];
    for i in 0..10 {
        array[i][i] = 1.0;
    }
    array
}

pub fn setij(arr: [[f64; 10]; 10], val: f64, i: usize, j: usize) -> [[f64; 10]; 10] {
    let mut new_arr = arr;
    new_arr[i][j] = val;
    new_arr
}

pub fn setij_ref(arr: &[[f64; 10]; 10], val: f64, i: usize, j: usize) -> [[f64; 10]; 10] {
    let mut new_arr = *arr;
    new_arr[i][j] = val;
    new_arr
}
