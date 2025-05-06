//! 迷宫生成器
//! 
//! 这个模块提供了一个迷宫生成器，可以生成各种尺寸的迷宫。
//! 
//! 迷宫的生成算法基于DFS算法，算法首先采用递归的方式编写，然后将尾递归改为了迭代的方式。
//! 
//! 迷宫的单元格类型包括：
//! - 墙壁
//! - 通道
//! - 起点
//! - 终点
//! 
//! 项目编译：
//! ```bash
//! wasm-pack build --target web
//! ```
//! 生成了pkg文件夹，里面包含wasm文件、package.json文件、ts文件、js文件
//! 
//! JavaScript代码示例：
//! ```javascript
//! const maze = new Maze(41, 41);
//! // Render the maze
//! const canvas = document.getElementById('canvas');
//! const ctx = canvas.getContext('2d');
//! for (let i = 0; i < maze.height; i++) {
//!     for (let j = 0; j < maze.width; j++) {
//!         const cell = maze.get_cell(i, j);
//!         if (cell === Cell.Wall) {
//!             ctx.fillRect(j * 10, i * 10, 10, 10);
//!         }
//!     }
//! }
//! ```
//! 
//! 另外注意：
//! 1. index.html需要某种http服务器运行，否则wasm文件无法加载，例如python -m http.server
//! 2. 更改`lib.rs`后，重新编译，要硬刷新浏览器页面（ctrl+F5），否则浏览器不会自动更新wasm文件
//! 3. 浏览器中运行时，如果迷宫较大，会有载入图示，请耐心等待
//! 4. `.cargo/config.toml`文件中的内容必不可少，因为`getrandom`依赖的`js`需要特别处理。
//！5. `web-sys`提供了`console.log`模块，可以用于输出日志在浏览器中。

use wasm_bindgen::prelude::*;
use rand::seq::SliceRandom;
use rand::thread_rng;
use web_sys::console;

// 在非测试环境中，使用WebAssembly的log,
// `wasm-pack build --target web --dev`
#[cfg(not(test))]
#[cfg(debug_assertions)]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

// 在非测试环境中，使用WebAssembly的log, 
//`wasm-pack build --target web`或者`wasm-pack build --target web --release`
#[cfg(not(test))]
#[cfg(not(debug_assertions))]
#[wasm_bindgen]
pub fn log(_s: &str) {}

// 在测试环境中，使用println! 输出日志
// `cargo test -- --show-output`
#[cfg(test)]
pub fn log(s: &str) {
    println!("{}", s);
}

/// 表示迷宫中的单元格类型
#[derive(Debug, Clone, Copy, PartialEq)]
#[wasm_bindgen]
pub enum Cell {
    /// 墙壁
    Wall,
    /// 通道
    Path,
    /// 起点
    Start,
    /// 终点
    End,
}

/// 表示一个迷宫
#[wasm_bindgen]
pub struct Maze {
    /// 迷宫的宽度（单元格数）
    width: usize,
    /// 迷宫的高度（单元格数）
    height: usize,
    /// 迷宫的网格数据
    grid: Vec<Vec<Cell>>,
    /// 起点的坐标 [y, x]
    start: [usize; 2],
    /// 终点的坐标 [y, x]
    end: [usize; 2],
}

#[wasm_bindgen]
impl Maze {
    /// 创建一个新的迷宫
    /// 
    /// # Arguments
    /// 
    /// * `width` - 迷宫的宽度（会被调整为奇数）
    /// * `height` - 迷宫的高度（会被调整为奇数）
    /// 
    /// # Returns
    /// 
    /// 返回一个新的迷宫实例
    #[wasm_bindgen(constructor)]
    pub fn new(width: usize, height: usize) -> Self {
        // 确保迷宫尺寸为奇数
        let width = if width % 2 == 0 { width + 1 } else { width };
        let height = if height % 2 == 0 { height + 1 } else { height };
        
        let mut maze = vec![vec![Cell::Wall; width]; height];
        
        // 设置入口和出口位置
        let start_x = 0;  // 左侧墙壁的右侧
        let start_y = 1;  // 距离顶部一格
        let end_x = width - 1;  // 右侧墙壁的左侧
        let end_y = height - 2;  // 距离底部一格
        
        // 确保入口和出口位置是路径
        maze[start_y][start_x] = Cell::Path;
        maze[end_y][end_x] = Cell::Path;
        
        // 生成迷宫路径
        Self::generate_paths_iter(&mut maze, 1, 1, width-1, height-1);
        
        // 设置入口和出口
        maze[start_y][start_x] = Cell::Start;
        maze[end_y][end_x] = Cell::End;
        
        Self {
            width,
            height,
            grid: maze,
            start: [start_y, start_x],
            end: [end_y, end_x],
        }
    }

    /// 递归生成迷宫路径
    /// 
    /// # Arguments
    /// 
    /// * `maze` - 迷宫网格
    /// * `x` - 当前单元格的x坐标
    /// * `y` - 当前单元格的y坐标
    /// * `width` - 迷宫的有效宽度
    /// * `height` - 迷宫的有效高度
    fn generate_paths(maze: &mut Vec<Vec<Cell>>, x: usize, y: usize, width: usize, height: usize) {
        // 将当前位置设为路径
        maze[y][x] = Cell::Path;
        
        // 定义四个方向
        let directions = [(0, 2), (2, 0), (0, -2), (-2, 0)];
        let mut dirs = directions.to_vec();
        dirs.shuffle(&mut thread_rng());
        
        // 尝试每个方向
        for (dx, dy) in dirs {
            let nx = x as isize + dx;
            let ny = y as isize + dy;
            
            // 检查新位置是否在范围内且是墙
            if nx > 0 && nx < width as isize && ny > 0 && ny < height as isize {
                let nx = nx as usize;
                let ny = ny as usize;
                if maze[ny][nx] == Cell::Wall {
                    // 打通路径
                    let mid_x = (x as isize + dx/2) as usize;
                    let mid_y = (y as isize + dy/2) as usize;
                    maze[mid_y][mid_x] = Cell::Path;
                    log(&format!("打通路径: ({}, {}) -> ({}, {}) -> ({}, {})", x, y, mid_x, mid_y, nx, ny));
                    Self::generate_paths(maze, nx, ny, width, height);
                }
            }
        }
    }

    /// 迭代生成迷宫路径
    /// 
    /// # Arguments
    /// 
    /// * `maze` - 迷宫网格
    /// * `start_x` - 起始单元格的x坐标
    /// * `start_y` - 起始单元格的y坐标
    /// * `width` - 迷宫的有效宽度
    /// * `height` - 迷宫的有效高度
    fn generate_paths_iter(maze: &mut Vec<Vec<Cell>>, start_x: usize, start_y: usize, width: usize, height: usize) {
        let mut stack = vec![(start_x, start_y)];
        
        while let Some((x, y)) = stack.pop() {
            // 将当前位置设为路径
            maze[y][x] = Cell::Path;
            
            // 定义四个方向
            let directions = [(0, 2), (2, 0), (0, -2), (-2, 0)];
            let mut dirs = directions.to_vec();
            dirs.shuffle(&mut thread_rng());
            
            // 尝试每个方向
            for (dx, dy) in dirs {
                let nx = x as isize + dx;
                let ny = y as isize + dy;
                
                // 检查新位置是否在范围内且是墙
                if nx > 0 && nx < width as isize && ny > 0 && ny < height as isize {
                    let nx = nx as usize;
                    let ny = ny as usize;
                    if maze[ny][nx] == Cell::Wall {
                        // 打通路径
                        let mid_x = (x as isize + dx/2) as usize;
                        let mid_y = (y as isize + dy/2) as usize;
                        maze[mid_y][mid_x] = Cell::Path;
                        log(&format!("打通路径: ({}, {}) -> ({}, {}) -> ({}, {})", x, y, mid_x, mid_y, nx, ny));
                        
                        // 将新位置压入栈顶，这样它会成为下一个处理的位置
                        stack.push((x, y));  // 先压入当前位置
                        stack.push((nx, ny)); // 再压入新位置
                        break; // 只处理一个方向，模拟递归行为
                    }
                }
            }
        }
    }

    /// 获取指定位置的单元格
    /// 
    /// # Arguments
    /// 
    /// * `x` - 行索引
    /// * `y` - 列索引
    /// 
    /// # Returns
    /// 
    /// 如果坐标有效，返回对应的单元格；否则返回 None
    #[wasm_bindgen]
    pub fn get_cell(&self, x: usize, y: usize) -> Option<Cell> {
        if x < self.height && y < self.width {
            Some(self.grid[x][y])
        } else {
            None
        }
    }

    /// 获取迷宫的尺寸
    /// 
    /// # Returns
    /// 
    /// 返回迷宫的宽度和高度 [width, height]
    #[wasm_bindgen]
    pub fn get_dimensions(&self) -> Vec<usize> {
        vec![self.width, self.height]
    }

    /// 获取起点的坐标
    /// 
    /// # Returns
    /// 
    /// 返回起点的坐标 [y, x]
    #[wasm_bindgen]
    pub fn get_start(&self) -> Vec<usize> {
        self.start.to_vec()
    }

    /// 获取终点的坐标
    /// 
    /// # Returns
    /// 
    /// 返回终点的坐标 [y, x]
    #[wasm_bindgen]
    pub fn get_end(&self) -> Vec<usize> {
        self.end.to_vec()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maze_generation() {
        let maze = Maze::new(41, 41);
        assert_eq!(maze.get_dimensions(), vec![41, 41]);
        assert_eq!(maze.get_start().len(), 2);
        assert_eq!(maze.get_end().len(), 2);

        // 打印迷宫，注意坐标顺序：y是行，x是列
        for y in 0..maze.height {
            for x in 0..maze.width {
                match maze.get_cell(y, x) {  // 注意这里交换了x和y的顺序
                    Some(Cell::Wall) => print!("██"),
                    Some(Cell::Path) => print!("  "),
                    Some(Cell::Start) => print!("S "),
                    Some(Cell::End) => print!("E "),
                    None => print!("? "),
                };
            }
            println!();
        }
    }
}
