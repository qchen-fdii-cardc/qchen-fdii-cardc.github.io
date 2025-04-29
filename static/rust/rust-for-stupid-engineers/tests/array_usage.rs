#[cfg(test)]
mod tests {
    use core::{alloc::Layout, ptr::NonNull};
    use rand::prelude::*;
    use rust_for_stupid_engineers::array_usage::{eye10, setij};
    use stacker;
    use std::alloc::alloc;

    #[test]
    fn basic_array_usage() {
        let mut array = [[0.0; 10]; 10];
        for i in 0..10 {
            array[i][i] = 1.0;
        }
        println!("{:?}", array);
    }

    #[test]
    fn test_eye10() {
        let mut arr10 = eye10();
        println!("{:?}", arr10);

        // arr10[0][0] = 100.00;
        arr10[1][1] = 100.0;
        println!("{:?}", arr10);
    }

    #[test]
    fn test_set11() {
        let arr1 = eye10();

        let (i, j, new_val) = (1, 1, 100.0);
        let arr2 = setij(arr1, new_val, i, j);
        assert_eq!(arr1[i][j], new_val);
        println!("{:?}", arr1);
        println!("{:?}", arr2);
    }

    #[test]
    fn test_small_array_usage() {
        let mut rng = rand::rng();
        let mut array = [0; 10];

        for i in 0..10 {
            array[i] = rng.random_range(1..100);
        }

        assert_eq!(array.len(), 10);
        assert!(array.iter().all(|&x| x >= 1 && x < 100));
    }

    #[test]
    fn fuck_stack_with_big_array() {
        let size = stacker::remaining_stack().unwrap();
        println!("stacker::remaining_stack: {}", size);
        const N: usize = 1199536;
        let array = [0; N];
        assert_eq!(array[0], 0);
        assert_eq!(array[N - 1], 0);
        assert!(array.len() < size / 4);
    }

    #[test]
    fn test_big_array_usage() {
        const N: usize = 1024;
        let mut array: Box<[[f64; N]; N]> = Box::new([[0.0; N]; N]);
        let mut rng = rand::rng();
        for i in 0..N {
            for j in 0..N {
                array[i][j] = rng.random_range(1.0..100.0);
            }
        }

        assert_eq!(array.len(), N);
        assert_eq!(array[0].len(), N);
    }
    fn heap_array<T, const N: usize>() -> Box<[T; N]> {
        unsafe {
            let layout = Layout::new::<[T; N]>();
            let pointer = alloc(layout);

            if pointer.is_null() {
                panic!("allocation failed");
            } else {
                let ptr = NonNull::new_unchecked(pointer as *mut [T; N]);
                Box::from_raw(ptr.as_ptr())
            }
        }
    }

    #[test]
    fn test_create_array_on_heap() {
        const N: usize = 8 * 1024 * 1024;
        let array: Box<[f64; N]> = unsafe {
            let layout = Layout::new::<[f64; N]>();
            let pointer = alloc(layout);
            if pointer.is_null() {
                panic!("allocation failed");
            } else {
                let ptr = NonNull::new_unchecked(pointer as *mut [f64; N]);
                Box::from_raw(ptr.as_ptr())
            }
        };
        assert_eq!(array[1], 0.0);
    }

    fn rand_array(array: &mut Vec<Vec<f64>>, rng: &mut impl Rng) {
        for i in 0..array.len() {
            for j in 0..array[i].len() {
                array[i][j] = rng.random_range(1.0..100.0);
            }
        }
    }
    #[test]
    fn test_very_big_vec() {
        let N = 8 * 1024 * 1024; // 8 MB
        let mut rng = rand::rng();

        let mut array = vec![vec![0.0; N]; 2];

        rand_array(&mut array, &mut rng);

        assert_eq!(array.len(), 2);
        assert_eq!(array[0].len(), N);
        assert!(array[0].iter().all(|&x| x >= 1.0 && x < 100.0));
    }
}
