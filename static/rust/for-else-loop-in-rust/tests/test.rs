use forelse::for_else;

/// Basic tests for the for-else macro
mod basic {
    use super::*;

    /// Test finding an element in a range
    #[test]
    fn test_find_in_range() {
        let mut found = false;
        for_else!(x in 1..=5 => {
            if x == 3 {
                println!("Found 3");
                found = true;
            }
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }

    /// Test not finding an element in a range
    #[test]
    fn test_not_find_in_range() {
        let mut found = false;
        for_else!(x in 1..=5 => {
            if x == 6 {
                println!("Found 6");
                found = true;
            }
        } else {
            println!("Not found");
        });
        assert!(!found);
    }

    /// Test with an empty range
    #[test]
    fn test_empty_range() {
        let mut found = false;
        for_else!(x in 1..1 => {
            println!("Found {}", x);
            found = true;
        } else {
            println!("Empty range");
        });
        assert!(!found);
    }
}

/// Tests for control flow statements
mod control_flow {
    use super::*;

    /// Test with break statement
    #[test]
    fn test_break() {
        let mut found = false;
        for_else!(x in 1..=5 => {
            if x == 3 {
                break;
            }
        } else {
            println!("Not found");
        });
        assert!(!found);
    }

    /// Test with continue statement
    #[test]
    fn test_continue() {
        let mut found = false;
        for_else!(x in 1..=5 => {
            if x == 3 {
                continue;
            }
            found = true;
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }
}

/// Tests for different iterator types
mod iterators {
    use super::*;

    /// Test with complex pattern matching
    #[test]
    fn test_tuple_iteration() {
        let mut found = false;
        for_else!((x, y) in [(1, 2), (3, 4), (5, 6)].iter() => {
            if x == &3 && y == &4 {
                println!("Found ({}, {})", x, y);
                found = true;
            }
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }

    /// Test with string iteration
    #[test]
    fn test_string_iteration() {
        let mut found = false;
        for_else!(c in "hello".chars() => {
            if c == 'e' {
                println!("Found 'e'");
                found = true;
            }
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }
}

/// Tests for fallible types (Option and Result)
mod fallible {
    use super::*;

    /// Test with Some variant
    #[test]
    fn test_some_iteration() {
        let mut found = false;
        for_else!(x in Some(42) => {
            if x == 42 {
                println!("Found 42");
                found = true;
            }
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }

    /// Test with None variant
    #[test]
    fn test_none_iteration() {
        let mut found = false;
        for_else!(x in None::<i32> => {
            println!("Found {}", x);
            found = true;
        } else {
            println!("None case");
        });
        assert!(!found);
    }

    /// Test with Ok variant
    #[test]
    fn test_ok_iteration() {
        let mut found = false;
        for_else!(x in Ok::<i32, &str>(42) => {
            if x == 42 {
                println!("Found 42");
                found = true;
            }
        } else {
            panic!("Should not reach else branch");
        });
        assert!(found);
    }

    /// Test with Err variant
    #[test]
    fn test_err_iteration() {
        let mut found = false;
        for_else!(x in Err::<i32, &str>("error") => {
            println!("Found {}", x);
            found = true;
        } else {
            println!("Error case");
        });
        assert!(!found);
    }
} 