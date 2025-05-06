// 迷宫单元格类型
const Cell = {
    Wall: 0,
    Path: 1,
    Start: 2,
    End: 3
};

// 迷宫类
class Maze {
    constructor(width, height) {
        // 确保迷宫尺寸为奇数
        this.width = width % 2 === 0 ? width + 1 : width;
        this.height = height % 2 === 0 ? height + 1 : height;

        // 初始化迷宫网格
        this.grid = Array(this.height).fill().map(() => Array(this.width).fill(Cell.Wall));

        // 设置入口和出口位置
        this.start = [1, 0];  // [y, x]
        this.end = [this.height - 2, this.width - 1];  // [y, x]

        // 确保入口和出口位置是路径
        this.grid[this.start[0]][this.start[1]] = Cell.Path;
        this.grid[this.end[0]][this.end[1]] = Cell.Path;

        // 生成迷宫路径
        this.generatePaths(1, 1);

        // 设置入口和出口
        this.grid[this.start[0]][this.start[1]] = Cell.Start;
        this.grid[this.end[0]][this.end[1]] = Cell.End;
    }

    // 获取指定位置的单元格
    getCell(x, y) {
        if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
            return this.grid[y][x];
        }
        return Cell.Wall;
    }

    // 获取迷宫尺寸
    getDimensions() {
        return [this.width, this.height];
    }

    // 获取起点坐标
    getStart() {
        return this.start;
    }

    // 获取终点坐标
    getEnd() {
        return this.end;
    }

    // 生成迷宫路径
    generatePaths(startX, startY) {
        const stack = [[startX, startY]];

        while (stack.length > 0) {
            const [x, y] = stack.pop();
            this.grid[y][x] = Cell.Path;

            // 定义四个方向：右、下、左、上
            const directions = [
                [0, 2],   // 右
                [2, 0],   // 下
                [0, -2],  // 左
                [-2, 0]   // 上
            ];

            // 随机打乱方向
            this.shuffleArray(directions);

            // 尝试每个方向
            for (const [dx, dy] of directions) {
                const nx = x + dx;
                const ny = y + dy;

                // 检查新位置是否在范围内且是墙
                if (nx > 0 && nx < this.width - 1 && ny > 0 && ny < this.height - 1) {
                    if (this.grid[ny][nx] === Cell.Wall) {
                        // 打通路径
                        const midX = x + dx / 2;
                        const midY = y + dy / 2;
                        this.grid[midY][midX] = Cell.Path;

                        // 将新位置压入栈顶
                        stack.push([x, y]);  // 先压入当前位置
                        stack.push([nx, ny]); // 再压入新位置
                        break; // 只处理一个方向，模拟递归行为
                    }
                }
            }
        }
    }

    // 辅助函数：随机打乱数组
    shuffleArray(array) {
        for (let i = array.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
    }
}

// 导出模块
export { Maze, Cell };
