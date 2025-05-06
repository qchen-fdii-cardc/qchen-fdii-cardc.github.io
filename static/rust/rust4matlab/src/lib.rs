#[no_mangle]
pub extern "C" fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[no_mangle]
pub extern "C" fn square(x: f64) -> f64 {
    x * x
}

#[no_mangle]
pub extern "C" fn linspace(start: f64, end: f64, n: usize, out_ptr: *mut f64) -> i32 {
    let mut result = Vec::new();
    let mut x = start;
    let step = (end - start) / (n - 1) as f64;
    for _i in 0..n {
        result.push(x);
        x += step;
    }
    
    let len = result.len();
    if !out_ptr.is_null() {
        unsafe {
            std::ptr::copy_nonoverlapping(result.as_ptr(), out_ptr, len);
        }
    }
    len as i32
}

#[no_mangle]
pub extern "C" fn free_linspace_result(ptr: *mut f64) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test] 
    fn test_linspace() {
        let mut output = vec![0.0; 11];
        let len = linspace(0.0, 1.0, 11, output.as_mut_ptr());
        assert_eq!(len, 11);
        for (idx, &ret) in output.iter().enumerate() {
            let expected = idx as f64 * 0.1;
            assert!((ret - expected).abs() <= f64::EPSILON);
        }   
    }

    #[test]
    fn test_square() {
        assert_eq!(square(2.0), 4.0);
        assert_eq!(square(-3.0), 9.0);
        assert_eq!(square(0.0), 0.0);
    }
}
