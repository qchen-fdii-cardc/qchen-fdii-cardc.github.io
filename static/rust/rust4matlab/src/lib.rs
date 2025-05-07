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
    if out_ptr.is_null() {
        return 0;
    }
    
    let step = if n > 1 {
        (end - start) / (n - 1) as f64
    } else {
        0.0
    };
    
    unsafe {
        for i in 0..n {
            *out_ptr.add(i) = start + step * i as f64;
        }
    }
    
    n as i32
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
