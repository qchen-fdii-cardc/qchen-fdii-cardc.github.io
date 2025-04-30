#[cfg(test)]
mod maze_test {
    use rust_for_stupid_engineers::maze::{DFSMaze, Maze2D};

    #[test]
    fn create_dfs_maze() {
        let maze = DFSMaze::new(30, 30);

        println!("{:?}", maze);

        println!("{}", maze);
    }
}
