use rand::prelude::SliceRandom;
use std::fmt;
use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MazeCell {
    Wall,
    Path,
    Start,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    i: isize, // in width, could be negative to represent delta
    j: isize, // in height, could be negative to represent delta
}

impl Add for Pos {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Pos {
            i: self.i + other.i,
            j: self.j + other.j,
        }
    }
}

pub trait Maze2D {
    fn new(width: usize, height: usize) -> Self;
    fn set_cell(&mut self, ij: &Pos, cell: MazeCell);
    fn get_cell(&self, ij: &Pos) -> MazeCell;
    fn set_start(&mut self, ij: &Pos);
    fn set_end(&mut self, ij: &Pos);
    fn get_start(&self) -> Pos;
    fn get_end(&self) -> Pos;
    fn width(&self) -> usize;
    fn height(&self) -> usize;

    fn is_in_bounds(&self, ij: &Pos) -> bool {
        let (x, y) = (ij.i, ij.j);
        x < self.width() as isize && y < self.height() as isize && ij.i >= 0 && ij.j >= 0
    }
}

/// A maze generator using the Depth-First Search (DFS) algorithm.
/// This implementation uses a Vec<Vec<MazeCell>> to represent the maze grid.
/// Outside the maze, cells are represented as walls.
#[derive(Debug, Clone)]
pub struct DFSMaze {
    width: usize,
    height: usize,
    cells: Vec<Vec<MazeCell>>, // 2D grid of cells, column-major, index as [i][j]
    start: Pos,
    end: Pos,
}

impl fmt::Display for DFSMaze {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for j in 0..self.height {
            for i in 0..self.width {
                let cell = self.get_cell(&Pos{ i: i as isize, j: j as isize });
                let symbol = match cell {
                    MazeCell::Wall => "██",
                    MazeCell::Path => "  ",
                    MazeCell::Start => "S>",
                    MazeCell::End => ">E",
                };
                write!(f, "{}", symbol)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl DFSMaze {
    /// DFS set path from start to end
    /// looking 2 cell in each direction
    fn get_neighbors(&self, pos: &Pos) -> Vec<Pos> {
        let mut neighbors = Vec::new();
        let directions = [
            Pos { i: 0, j: 2 },  // right
            Pos { i: 2, j: 0 },  // down
            Pos { i: 0, j: -2 }, // left
            Pos { i: -2, j: 0 }, // up
        ];

        for dir in directions.iter() {
            let new_pos = *pos + *dir;
            if new_pos.i > 0 && new_pos.i < self.width() as isize - 1
                && new_pos.j > 0
                && new_pos.j < self.height() as isize - 1
                && self.get_cell(&new_pos) == MazeCell::Wall
            {
                neighbors.push(new_pos);
            }
        }
        neighbors
    }
    fn generate_maze_path(&mut self) {
        let mut stack = Vec::new();
        stack.push(self.start + Pos { i: 1, j: 0 }); // Start from the first path cell

        while let Some(current) = stack.pop() {
            self.cells[current.i as usize][current.j as usize] = MazeCell::Path;

            let mut neighbors = self.get_neighbors(&current);
            neighbors.shuffle(&mut rand::rng()); // Shuffle neighbors for randomness

            for next in neighbors {
                // Check if the next cell is still a wall
                if self.get_cell(&next) == MazeCell::Wall {
                    // Carve the wall between current and next
                    let wall = Pos {
                        i: (current.i + next.i) / 2,
                        j: (current.j + next.j) / 2,
                    };
                    self.cells[wall.i as usize][wall.j as usize] = MazeCell::Path;

                    // Mark the next cell as a path and push it onto the stack
                    self.cells[next.i as usize][next.j as usize] = MazeCell::Path;
                    stack.push(next);
                }
            }
        }
    }
}

impl Maze2D for DFSMaze {
    fn new(width: usize, height: usize) -> Self {
        // make sure width and height are odd numbers
        let width = if width % 2 == 0 { width + 1 } else { width };
        let height = if height % 2 == 0 { height + 1 } else { height };
        // i, horizontally left to right,  from 0 to width-1
        // j, vertically top to bottom, from 0 to height-1
        let mut cells = vec![vec![MazeCell::Wall; height]; width];
        let start = Pos { i: 0, j: 1 };
        let end = Pos {
            i: (width - 1) as isize,
            j: (height - 2) as isize,
        };
        cells[start.i as usize][start.j as usize] = MazeCell::Start;
        cells[end.i as usize][end.j as usize] = MazeCell::End;
        let mut maze = DFSMaze {
            width,
            height,
            cells,
            start,
            end,
        };


        // initialize path from start to end
        maze.generate_maze_path();

        maze
    }

    fn set_cell(&mut self, ij: &Pos, cell: MazeCell) {
        if self.is_in_bounds(ij) {
            match cell {
                MazeCell::Start => {
                    // Guarantee only one start
                    let old_start = self.get_start();
                    if old_start != *ij {
                        self.cells[old_start.i as usize][old_start.j as usize] = cell;
                        self.cells[ij.i as usize][ij.j as usize] = MazeCell::Start;
                    }
                }
                MazeCell::End => {
                    // Guarantee only one end
                    let old_end = self.get_end();
                    if old_end != *ij {
                        self.cells[old_end.i as usize][old_end.j as usize] = MazeCell::End;
                        self.cells[ij.i as usize][ij.j as usize] = MazeCell::End;
                    }
                }
                _ => {
                    self.cells[ij.i as usize][ij.j as usize] = cell;
                }
            }
        }
    }

    fn get_cell(&self, ij: &Pos) -> MazeCell {
        if self.is_in_bounds(ij) {
            self.cells[ij.i as usize][ij.j as usize]
        } else {
            MazeCell::Wall
        }
    }

    fn set_start(&mut self, ij: &Pos) {
        self.set_cell(ij, MazeCell::Start);
    }

    fn set_end(&mut self, ij: &Pos) {
        self.set_cell(ij, MazeCell::End);
    }

    fn get_start(&self) -> Pos {
        self.start
    }

    fn get_end(&self) -> Pos {
        self.end
    }

    fn width(&self) -> usize {
        self.width
    }

    fn height(&self) -> usize {
        self.height
    }
}
