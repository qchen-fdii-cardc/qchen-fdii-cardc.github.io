// Core game logic without UI dependencies
class GameCore {
    constructor(size = 4) {
        this.size = size;
        this.grid = this.createEmptyGrid();
        this.score = 0;
        this.strategy = null;
        this.addRandomTile();
        this.addRandomTile();
    }

    createEmptyGrid() {
        return Array.from({ length: this.size }, () => Array(this.size).fill(0));
    }

    addRandomTile() {
        const emptyCells = [];
        for (let r = 0; r < this.size; r++) {
            for (let c = 0; c < this.size; c++) {
                if (this.grid[r][c] === 0) emptyCells.push([r, c]);
            }
        }
        if (emptyCells.length > 0) {
            const [row, col] = emptyCells[Math.floor(Math.random() * emptyCells.length)];
            this.grid[row][col] = Math.random() < 0.9 ? 2 : 4;
            return { row, col, value: this.grid[row][col] };
        }
        return null;
    }

    move(direction) {
        const previousGrid = JSON.stringify(this.grid);
        let scoreIncrease = 0;

        if (direction === 'up') {
            this.rotateGrid(false);
            for (let r = 0; r < this.size; r++) {
                const { row: newRow, scoreIncrease: rowScore } = this.slide(this.grid[r]);
                this.grid[r] = newRow;
                scoreIncrease += rowScore;
            }
            this.rotateGrid(true);
        } else if (direction === 'down') {
            this.rotateGrid(true);
            for (let r = 0; r < this.size; r++) {
                const { row: newRow, scoreIncrease: rowScore } = this.slide(this.grid[r]);
                this.grid[r] = newRow;
                scoreIncrease += rowScore;
            }
            this.rotateGrid(false);
        } else if (direction === 'left' || direction === 'right') {
            for (let r = 0; r < this.size; r++) {
                const row = direction === 'right' ? [...this.grid[r]].reverse() : this.grid[r];
                const { row: newRow, scoreIncrease: rowScore } = this.slide(row);
                this.grid[r] = direction === 'right' ? newRow.reverse() : newRow;
                scoreIncrease += rowScore;
            }
        }

        const changed = previousGrid !== JSON.stringify(this.grid);
        if (changed) {
            this.score += scoreIncrease;
            const newTile = this.addRandomTile();
            return { changed, scoreIncrease, newTile };
        }
        return { changed, scoreIncrease: 0, newTile: null };
    }

    slide(row) {
        const filtered = row.filter(val => val !== 0);
        const merged = [];
        let scoreIncrease = 0;

        for (let i = 0; i < filtered.length; i++) {
            if (filtered[i] === filtered[i + 1]) {
                const mergedValue = filtered[i] * 2;
                merged.push(mergedValue);
                scoreIncrease += mergedValue;
                i++;
            } else {
                merged.push(filtered[i]);
            }
        }

        while (merged.length < this.size) {
            merged.push(0);
        }

        return { row: merged, scoreIncrease };
    }

    rotateGrid(clockwise) {
        const size = this.grid.length;
        const newGrid = Array.from({ length: size }, () => Array(size).fill(0));

        for (let r = 0; r < size; r++) {
            for (let c = 0; c < size; c++) {
                if (clockwise) {
                    newGrid[c][size - 1 - r] = this.grid[r][c];
                } else {
                    newGrid[size - 1 - c][r] = this.grid[r][c];
                }
            }
        }

        for (let r = 0; r < size; r++) {
            for (let c = 0; c < size; c++) {
                this.grid[r][c] = newGrid[r][c];
            }
        }
    }

    isGameOver() {
        // Check for empty cells
        for (let r = 0; r < this.size; r++) {
            for (let c = 0; c < this.size; c++) {
                if (this.grid[r][c] === 0) return false;
            }
        }

        // Check for possible merges
        for (let r = 0; r < this.size; r++) {
            for (let c = 0; c < this.size; c++) {
                if (r > 0 && this.grid[r][c] === this.grid[r - 1][c]) return false;
                if (c > 0 && this.grid[r][c] === this.grid[r][c - 1]) return false;
            }
        }

        return true;
    }

    getState() {
        return {
            grid: JSON.parse(JSON.stringify(this.grid)),
            score: this.score,
            size: this.size,
            isOver: this.isGameOver()
        };
    }

    reset() {
        this.grid = this.createEmptyGrid();
        this.score = 0;
        this.addRandomTile();
        this.addRandomTile();
    }

    getScore() {
        return this.score;
    }

    getNextMove() {
        if (this.strategy) {
            return this.strategy.getNextMoveFromGrid(this.grid);
        }
        return null;
    }

    setStrategy(strategy) {
        this.strategy = strategy;
    }

    getStrategy() {
        return this.strategy;
    }
}

// Strategy interface (pure logic, no UI dependencies)
class GameStrategy {
    constructor() {
        this.name = "Base Strategy";
    }

    getNextMove(gameCore) {
        // Default implementation should be overridden by subclasses
        return this.getNextMoveFromGrid(gameCore.grid);
    }

    getNextMoveFromGrid(grid) {
        // This is the method that subclasses should override
        throw new Error('Strategy must implement getNextMoveFromGrid');
    }
}

// Sequence Strategy (Abstract base class for sequence-based strategies)
class SequenceStrategy extends GameStrategy {
    constructor(sequence, name) {
        super();
        if (typeof sequence === "string") {
            switch (sequence) {
                case "clockwise":
                    this.sequence = ['up', 'right', 'down', 'left'];
                    this.name = "Clockwise";
                    break;
                case "counter-clockwise":
                    this.sequence = ['up', 'left', 'down', 'right'];
                    this.name = "Counter-Clockwise";
                    break;
                case "snake":
                    this.sequence = ['left', 'down', 'left', 'down', 'right', 'down'];
                    this.name = "Snake";
                    break;
                default:
                    throw new Error(`Unknown sequence type: ${sequence}`);
            }
        } else {
            this.sequence = sequence;
            this.name = name;
        }
        this.currentIndex = 0;
    }

    getNextMoveFromGrid(grid) {
        const move = this.sequence[this.currentIndex];
        this.currentIndex = (this.currentIndex + 1) % this.sequence.length;
        return move;
    }
}

// Corner Strategy
class CornerStrategy extends GameStrategy {
    constructor() {
        super();
        this.name = "Corner";
        this.targetCorner = { row: 0, col: 0 };
        this.lastMove = null;
        this.sameMovesCount = 0;
        this.moveScores = {};
        this.lastGrid = null;
        this.cachedScores = {};
    }

    getNextMoveFromGrid(grid) {
        const moves = ['left', 'down', 'up', 'right'];
        this.moveScores = {};

        // Check if the grid has changed since last evaluation
        const gridString = JSON.stringify(grid);
        if (this.lastGrid === gridString && Object.keys(this.cachedScores).length > 0) {
            this.moveScores = { ...this.cachedScores };
            const possibleMoves = Object.entries(this.moveScores)
                .filter(([_, score]) => score > -100)
                .map(([move]) => move);

            if (possibleMoves.length === 0) return null;

            const bestMoves = possibleMoves.sort((a, b) => this.moveScores[b] - this.moveScores[a]);
            const bestMove = bestMoves[0];

            if (bestMove === this.lastMove) {
                this.sameMovesCount++;
                if (this.sameMovesCount > 3 && bestMoves.length > 1) {
                    this.sameMovesCount = 0;
                    this.lastMove = bestMoves[1];
                    return bestMoves[1];
                }
            } else {
                this.sameMovesCount = 0;
                this.lastMove = bestMove;
            }

            return bestMove;
        }

        // First, find valid moves
        const possibleMoves = moves.filter(move => this.canMove(grid, move));

        if (possibleMoves.length === 0) {
            return null;
        }

        // Only evaluate possible moves
        for (const move of possibleMoves) {
            this.moveScores[move] = this.evaluateMove(grid, move);
        }

        // Cache the scores
        this.lastGrid = gridString;
        this.cachedScores = { ...this.moveScores };

        // Sort possible moves by their scores
        const bestMoves = possibleMoves.sort((a, b) => this.moveScores[b] - this.moveScores[a]);
        const bestMove = bestMoves[0];

        // Update move tracking
        if (bestMove === this.lastMove) {
            this.sameMovesCount++;
            if (this.sameMovesCount > 3 && possibleMoves.length > 1) {
                this.sameMovesCount = 0;
                this.lastMove = bestMoves[1];
                return bestMoves[1];
            }
        } else {
            this.sameMovesCount = 0;
            this.lastMove = bestMove;
        }

        return bestMove;
    }

    evaluateMove(grid, direction) {
        const size = grid.length;
        let score = 0;

        // Create a deep copy of the grid to simulate the move
        const gridCopy = JSON.parse(JSON.stringify(grid));
        const previousState = JSON.stringify(gridCopy);

        // Simulate the move
        this.simulateMove(gridCopy, direction);

        // If the move doesn't change the grid, return a low score but not -Infinity
        // This allows the move to still be considered if it's the only option
        if (previousState === JSON.stringify(gridCopy)) {
            return -100;
        }

        // 1. Evaluate corner preference (top-left corner strategy)
        const cornerValue = gridCopy[this.targetCorner.row][this.targetCorner.col];
        score += cornerValue * 3;

        // 2. Evaluate monotonic decrease from the corner
        score += this.evaluateMonotonicDecrease(gridCopy) * 2;

        // 3. Evaluate mergeability
        score += this.evaluateMergeability(gridCopy) * 1.5;

        // 4. Evaluate empty cells (weighted more heavily)
        score += this.countEmptyCells(gridCopy) * 20;

        // 5. Penalize moves that put small values next to large ones
        score -= this.evaluateValueDisparity(gridCopy);

        // 6. Bonus for moves that align with the last successful move
        if (direction === this.lastMove && this.sameMovesCount < 3) {
            score += 50;
        }

        return score;
    }

    findHighestTilePosition(grid) {
        let maxVal = 0;
        let pos = { row: 0, col: 0 };

        for (let i = 0; i < grid.length; i++) {
            for (let j = 0; j < grid.length; j++) {
                if (grid[i][j] > maxVal) {
                    maxVal = grid[i][j];
                    pos = { row: i, col: j };
                }
            }
        }

        return pos;
    }

    evaluateValueDisparity(grid) {
        const size = grid.length;
        let penalty = 0;

        for (let i = 0; i < size; i++) {
            for (let j = 0; j < size; j++) {
                if (grid[i][j] === 0) continue;

                // Check adjacent cells
                const adjacentPositions = [
                    [i - 1, j], [i + 1, j], [i, j - 1], [i, j + 1]
                ];

                for (const [r, c] of adjacentPositions) {
                    if (r >= 0 && r < size && c >= 0 && c < size && grid[r][c] !== 0) {
                        const ratio = Math.max(grid[i][j], grid[r][c]) / Math.min(grid[i][j], grid[r][c]);
                        if (ratio > 4) { // Penalize when difference is more than 4x
                            penalty += Math.log2(ratio);
                        }
                    }
                }
            }
        }

        return penalty;
    }

    evaluateMonotonicDecrease(grid) {
        const size = grid.length;
        let score = 0;
        const maxValue = Math.max(...grid.flat());

        // Start from the corner
        let expectedValue = maxValue;

        // Check rows
        for (let i = 0; i < size; i++) {
            for (let j = 0; j < size; j++) {
                if (grid[i][j] <= expectedValue) {
                    score += 10;
                    expectedValue = grid[i][j];
                }
            }
        }

        return score;
    }

    evaluateMergeability(grid) {
        const size = grid.length;
        let score = 0;

        // Check horizontal mergeability
        for (let i = 0; i < size; i++) {
            for (let j = 0; j < size - 1; j++) {
                if (grid[i][j] === grid[i][j + 1] && grid[i][j] !== 0) {
                    score += Math.log2(grid[i][j]);
                }
            }
        }

        // Check vertical mergeability
        for (let j = 0; j < size; j++) {
            for (let i = 0; i < size - 1; i++) {
                if (grid[i][j] === grid[i + 1][j] && grid[i][j] !== 0) {
                    score += Math.log2(grid[i][j]);
                }
            }
        }

        return score;
    }

    countEmptyCells(grid) {
        let count = 0;
        for (let i = 0; i < grid.length; i++) {
            for (let j = 0; j < grid.length; j++) {
                if (grid[i][j] === 0) count++;
            }
        }
        return count;
    }

    simulateMove(grid, direction) {
        const size = grid.length;

        if (direction === 'left' || direction === 'right') {
            for (let r = 0; r < size; r++) {
                let row = [...grid[r]];  // Create a copy of the row
                if (direction === 'right') row = row.reverse();

                // Filter and merge
                row = row.filter(val => val !== 0);
                const merged = [];
                for (let i = 0; i < row.length; i++) {
                    if (i + 1 < row.length && row[i] === row[i + 1]) {
                        merged.push(row[i] * 2);
                        i++;
                    } else {
                        merged.push(row[i]);
                    }
                }

                // Pad with zeros
                while (merged.length < size) merged.push(0);
                if (direction === 'right') merged.reverse();
                grid[r] = merged;
            }
        } else {
            // Create a rotated copy for up/down moves
            const rotated = Array.from({ length: size }, () => Array(size).fill(0));

            // Rotate the grid
            for (let r = 0; r < size; r++) {
                for (let c = 0; c < size; c++) {
                    if (direction === 'up') {
                        rotated[c][r] = grid[r][c];
                    } else {
                        rotated[size - 1 - c][r] = grid[r][c];
                    }
                }
            }

            // Process each row
            for (let r = 0; r < size; r++) {
                let row = rotated[r].filter(val => val !== 0);
                const merged = [];
                for (let i = 0; i < row.length; i++) {
                    if (i + 1 < row.length && row[i] === row[i + 1]) {
                        merged.push(row[i] * 2);
                        i++;
                    } else {
                        merged.push(row[i]);
                    }
                }
                while (merged.length < size) merged.push(0);
                rotated[r] = merged;
            }

            // Rotate back
            for (let r = 0; r < size; r++) {
                for (let c = 0; c < size; c++) {
                    if (direction === 'up') {
                        grid[r][c] = rotated[c][r];
                    } else {
                        grid[r][c] = rotated[size - 1 - c][r];
                    }
                }
            }
        }
    }

    canMove(grid, direction) {
        const size = grid.length;
        if (direction === 'left' || direction === 'right') {
            for (let r = 0; r < size; r++) {
                for (let c = 0; c < size; c++) {
                    // Check for empty cells
                    if (grid[r][c] === 0) return true;
                    // Check for mergeable cells
                    if (c < size - 1 && grid[r][c] === grid[r][c + 1]) return true;
                    // For right move, also check if we can move tiles to empty spaces
                    if (direction === 'right' && c < size - 1 && grid[r][c] !== 0 && grid[r][c + 1] === 0) return true;
                    // For left move, check if we can move tiles to empty spaces
                    if (direction === 'left' && c > 0 && grid[r][c] !== 0 && grid[r][c - 1] === 0) return true;
                }
            }
        } else {
            for (let c = 0; c < size; c++) {
                for (let r = 0; r < size; r++) {
                    // Check for empty cells
                    if (grid[r][c] === 0) return true;
                    // Check for mergeable cells
                    if (r < size - 1 && grid[r][c] === grid[r + 1][c]) return true;
                    // For down move, also check if we can move tiles to empty spaces
                    if (direction === 'down' && r < size - 1 && grid[r][c] !== 0 && grid[r + 1][c] === 0) return true;
                    // For up move, check if we can move tiles to empty spaces
                    if (direction === 'up' && r > 0 && grid[r][c] !== 0 && grid[r - 1][c] === 0) return true;
                }
            }
        }
        return false;
    }
}

// Strategy UI Component
class StrategyUI {
    constructor(strategy) {
        this.strategy = strategy;
        this.debugPanel = document.getElementById('debug-panel');
        this.moveScoresPanel = document.getElementById('move-scores');
        this.debugTitle = document.getElementById('debug-info-title');
        if (this.debugPanel) {
            this.debugPanel.classList.add('active');
        }
        this.updateDebugTitle(`${strategy.name} Strategy`);
    }

    updateDebugTitle(title) {
        if (this.debugTitle) {
            this.debugTitle.textContent = title;
        }
    }

    displayMoveScores(moveScores) {
        if (!this.moveScoresPanel) return;

        if (this.strategy instanceof SequenceStrategy) {
            this.displaySequenceInfo();
        } else if (this.strategy instanceof CornerStrategy) {
            this.displayCornerInfo(moveScores);
        }
    }

    displaySequenceInfo() {
        if (!this.moveScoresPanel) return;

        const nextMoveIndex = this.strategy.currentIndex;
        this.moveScoresPanel.innerHTML = `
            <div class="sequence-display">
                ${this.strategy.sequence.map((move, index) => `
                    <div class="move-score ${index === nextMoveIndex ? 'best' : ''}">
                        ${move}${index === nextMoveIndex ? ' (next)' : ''}
                    </div>
                `).join('')}
            </div>
        `;
    }

    displayCornerInfo(moveScores) {
        if (!this.moveScoresPanel) return;

        const moves = Object.entries(moveScores);
        const bestScore = Math.max(...moves.map(([_, score]) => score));

        this.moveScoresPanel.innerHTML = moves
            .sort((a, b) => b[1] - a[1])
            .map(([move, score]) => `
                <div class="move-score ${score === bestScore ? 'best' : ''}">
                    ${move}: ${score === -Infinity ? 'impossible' : score.toFixed(1)}
                </div>
            `).join('');
    }
}

// Update AutoPlayUI to handle strategy UI
class AutoPlayUI {
    constructor(gameCore, gameUI, strategy) {
        this.game = gameCore;
        this.gameUI = gameUI;
        this.strategy = strategy;
        this.strategyUI = new StrategyUI(strategy);
        this.interval = null;
        this.moveDelay = 200;
        this.setupControls();
    }

    setStrategy(strategy) {
        this.strategy = strategy;
        this.strategyUI = new StrategyUI(strategy);
        if (this.interval) {
            this.stop();
        }
    }

    setupControls() {
        const autoPlayButton = document.getElementById('auto-play');
        const stopButton = document.getElementById('stop-auto-play');
        const speedSelector = document.getElementById('speed-selector');

        if (autoPlayButton) {
            autoPlayButton.addEventListener('click', () => {
                if (this.interval) {
                    this.stop();
                } else {
                    this.start();
                }
            });
        }

        if (stopButton) {
            stopButton.addEventListener('click', () => this.stop());
        }

        if (speedSelector) {
            speedSelector.addEventListener('change', (e) => {
                this.moveDelay = parseInt(e.target.value);
                if (this.interval) {
                    this.stop();
                    this.start();
                }
            });
        }
    }

    start() {
        const autoPlayButton = document.getElementById('auto-play');
        if (autoPlayButton) {
            autoPlayButton.classList.add('active');
        }

        this.interval = setInterval(() => {
            if (this.game.isGameOver()) {
                this.stop();
                this.gameUI.showGameOver();
                return;
            }

            if (this.strategy instanceof CornerStrategy) {
                const move = this.strategy.getNextMove(this.game);
                this.strategyUI.displayMoveScores(this.strategy.moveScores);
                const result = this.game.move(move);
                if (result.changed) {
                    this.gameUI.render();
                    if (result.newTile) {
                        this.gameUI.animateNewTile(result.newTile);
                    }
                }
            } else if (this.strategy instanceof SequenceStrategy) {
                const move = this.strategy.getNextMove(this.game);
                this.strategyUI.displayMoveScores();
                const result = this.game.move(move);
                if (result.changed) {
                    this.gameUI.render();
                    if (result.newTile) {
                        this.gameUI.animateNewTile(result.newTile);
                    }
                }
            }
        }, this.moveDelay);
    }

    stop() {
        if (this.interval) {
            clearInterval(this.interval);
            this.interval = null;
        }

        const autoPlayButton = document.getElementById('auto-play');
        if (autoPlayButton) {
            autoPlayButton.classList.remove('active');
        }
    }
}

// Tournament system (no UI dependencies)
class Tournament {
    constructor() {
        this.strategies = [
            new CornerStrategy(),
            new SequenceStrategy("clockwise"),
            new SequenceStrategy("counter-clockwise"),
            new SequenceStrategy("snake")
        ];
        this.results = new Map();
        this.gamesPerStrategy = 20; // default value
        this.gridSize = 4; // default value
        this.currentStrategy = null;
        this.currentGameNumber = 0;
        this.totalGames = this.strategies.length * this.gamesPerStrategy;
        this.onProgress = null;
    }

    setGridSize(size) {
        this.gridSize = size;
    }

    setGamesPerStrategy(games) {
        this.gamesPerStrategy = games;
        this.totalGames = this.strategies.length * this.gamesPerStrategy;
    }

    setProgressCallback(callback) {
        this.onProgress = callback;
    }

    calculateStats(scores) {
        const sum = scores.reduce((a, b) => a + b, 0);
        const avg = sum / scores.length;
        const variance = scores.reduce((a, b) => a + Math.pow(b - avg, 2), 0) / scores.length;
        const stdDev = Math.sqrt(variance);
        return {
            scores: scores,
            min: Math.min(...scores),
            max: Math.max(...scores),
            average: avg,
            median: this.calculateMedian(scores),
            stdDev: stdDev
        };
    }

    async runTournament() {
        this.results.clear();
        let gameNumber = 0;

        for (const strategy of this.strategies) {
            this.currentStrategy = strategy;
            const scores = [];

            for (let i = 0; i < this.gamesPerStrategy; i++) {
                this.currentGameNumber = gameNumber + 1;
                const game = new GameCore(this.gridSize);
                game.setStrategy(strategy);
                let moveCount = 0;
                let lastScore = 0;
                let noProgressCount = 0;
                const maxNoProgress = 50; // 如果50次移动没有分数变化，认为陷入死循环

                while (!game.isGameOver()) {
                    const move = strategy.getNextMove(game);
                    if (!move) break;

                    const result = game.move(move);
                    if (!result.changed) {
                        // 如果移动无效，尝试其他方向
                        const directions = ['up', 'right', 'down', 'left'];
                        let foundValidMove = false;
                        for (const dir of directions) {
                            if (dir !== move) {
                                const altResult = game.move(dir);
                                if (altResult.changed) {
                                    foundValidMove = true;
                                    break;
                                }
                            }
                        }
                        if (!foundValidMove) break; // 如果所有方向都无法移动，游戏结束
                    }

                    // 检查是否有分数进展
                    if (game.getScore() === lastScore) {
                        noProgressCount++;
                        if (noProgressCount >= maxNoProgress) {
                            // 强制检查是否真的无法继续
                            let canMove = false;
                            for (const dir of ['up', 'right', 'down', 'left']) {
                                const testResult = game.move(dir);
                                if (testResult.changed) {
                                    canMove = true;
                                    break;
                                }
                            }
                            if (!canMove) break;
                            noProgressCount = 0; // 重置计数器，给策略一次新的机会
                        }
                    } else {
                        noProgressCount = 0;
                        lastScore = game.getScore();
                    }

                    moveCount++;
                }

                const score = game.getScore();
                scores.push(score);

                if (this.onProgress) {
                    this.onProgress({
                        strategy: strategy.name,
                        gameNumber: this.currentGameNumber,
                        totalGames: this.totalGames,
                        lastGameScore: score,
                        progress: (this.currentGameNumber / this.totalGames) * 100
                    });
                }

                gameNumber++;
                // Add a small delay to allow UI updates
                await new Promise(resolve => setTimeout(resolve, 1));
            }

            this.results.set(strategy.name, this.calculateStats(scores));
        }

        return this.getResults();
    }

    calculateMedian(scores) {
        const sorted = [...scores].sort((a, b) => a - b);
        const mid = Math.floor(sorted.length / 2);
        return sorted.length % 2 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
    }

    getResults() {
        return this.results;
    }
}

// Tournament UI
class TournamentUI {
    constructor(tournament) {
        this.tournament = tournament;
        this.overlay = document.getElementById('tournament-overlay');
        this.currentStrategy = document.getElementById('current-strategy');
        this.currentGame = document.getElementById('current-game');
        this.totalGames = document.getElementById('total-games');
        this.lastGameScore = document.getElementById('last-game-score');
        this.progressBar = document.getElementById('tournament-progress-bar');
        this.gamesPerStrategySelect = document.getElementById('games-per-strategy');
        this.gridSizeSelect = document.getElementById('tournament-grid-size');

        this.setupEventListeners();
    }

    setupEventListeners() {
        document.getElementById('start-tournament').addEventListener('click', () => {
            this.startTournament();
        });

        if (this.gamesPerStrategySelect) {
            this.gamesPerStrategySelect.addEventListener('change', (e) => {
                const games = parseInt(e.target.value);
                this.tournament.setGamesPerStrategy(games);
            });
            // Set initial value
            this.tournament.setGamesPerStrategy(parseInt(this.gamesPerStrategySelect.value));
        }

        if (this.gridSizeSelect) {
            this.gridSizeSelect.addEventListener('change', (e) => {
                const size = parseInt(e.target.value);
                this.tournament.setGridSize(size);
            });
            // Set initial value
            this.tournament.setGridSize(parseInt(this.gridSizeSelect.value));
        }
    }

    showOverlay() {
        this.overlay.classList.add('active');
    }

    hideOverlay() {
        this.overlay.classList.remove('active');
    }

    updateProgress(data) {
        this.currentStrategy.textContent = data.strategy;
        this.currentGame.textContent = data.gameNumber;
        this.totalGames.textContent = data.totalGames;
        this.lastGameScore.textContent = data.lastGameScore;
        this.progressBar.style.width = `${data.progress}%`;
    }

    async startTournament() {
        this.showOverlay();
        this.tournament.setProgressCallback(data => this.updateProgress(data));

        try {
            const results = await this.tournament.runTournament();
            this.displayResults(results);
        } finally {
            this.hideOverlay();
        }
    }

    displayResults(results) {
        const sortedResults = Array.from(results.entries())
            .sort(([, a], [, b]) => b.average - a.average);

        let resultsHtml = '<h3>Tournament Results</h3><table>';
        resultsHtml += `
            <tr>
                <th>Strategy</th>
                <th>Average</th>
                <th>Median</th>
                <th>Min</th>
                <th>Max</th>
                <th>Std Dev</th>
            </tr>
        `;

        for (const [strategy, data] of sortedResults) {
            resultsHtml += `
                <tr>
                    <td>${strategy}</td>
                    <td>${Math.round(data.average)}</td>
                    <td>${Math.round(data.median)}</td>
                    <td>${Math.round(data.min)}</td>
                    <td>${Math.round(data.max)}</td>
                    <td>${Math.round(data.stdDev)}</td>
                </tr>
            `;
        }
        resultsHtml += '</table>';

        const resultsPanel = document.getElementById('tournament-results');
        resultsPanel.innerHTML = resultsHtml;
        resultsPanel.style.display = 'block';
    }
}

// Game UI Component
class GameUI {
    constructor(gameCore) {
        this.game = gameCore;
        this.grid = document.getElementById('game-grid');
        this.scoreDisplay = document.getElementById('score');
        this.gameOverDisplay = document.getElementById('game-over');
        this.finalScoreDisplay = document.getElementById('final-score');
        this.cells = [];
        this.setupGrid();
        this.setupKeyboardControls();
        this.setupTouchControls();
        this.render();
    }

    setupGrid() {
        this.grid.innerHTML = '';
        this.grid.style.gridTemplateColumns = `repeat(${this.game.size}, 1fr)`;
        this.cells = [];

        for (let i = 0; i < this.game.size; i++) {
            this.cells[i] = [];
            for (let j = 0; j < this.game.size; j++) {
                const cell = document.createElement('div');
                cell.className = 'cell';
                this.grid.appendChild(cell);
                this.cells[i][j] = cell;
            }
        }
    }

    setupKeyboardControls() {
        document.addEventListener('keydown', (e) => {
            if (e.key.startsWith('Arrow') || ['w', 'a', 's', 'd'].includes(e.key.toLowerCase())) {
                e.preventDefault();
                this.handleInput(e.key);
            }
        });
    }

    setupTouchControls() {
        let touchStartX = 0;
        let touchStartY = 0;
        const minSwipeDistance = 30;

        this.grid.addEventListener('touchstart', (e) => {
            touchStartX = e.touches[0].clientX;
            touchStartY = e.touches[0].clientY;
        });

        this.grid.addEventListener('touchmove', (e) => {
            e.preventDefault();
        });

        this.grid.addEventListener('touchend', (e) => {
            const touchEndX = e.changedTouches[0].clientX;
            const touchEndY = e.changedTouches[0].clientY;
            const dx = touchEndX - touchStartX;
            const dy = touchEndY - touchStartY;

            if (Math.abs(dx) < minSwipeDistance && Math.abs(dy) < minSwipeDistance) {
                return;
            }

            if (Math.abs(dx) > Math.abs(dy)) {
                this.handleInput(dx > 0 ? 'ArrowRight' : 'ArrowLeft');
            } else {
                this.handleInput(dy > 0 ? 'ArrowDown' : 'ArrowUp');
            }
        });
    }

    handleInput(key) {
        let direction = null;
        switch (key.toLowerCase()) {
            case 'arrowup':
            case 'w':
                direction = 'up';
                break;
            case 'arrowdown':
            case 's':
                direction = 'down';
                break;
            case 'arrowleft':
            case 'a':
                direction = 'left';
                break;
            case 'arrowright':
            case 'd':
                direction = 'right';
                break;
        }

        if (direction) {
            const result = this.game.move(direction);
            if (result.changed) {
                this.render();
                if (result.newTile) {
                    this.animateNewTile(result.newTile);
                }
                if (this.game.isGameOver()) {
                    this.showGameOver();
                }
            }
        }
    }

    render() {
        const state = this.game.getState();
        this.scoreDisplay.textContent = state.score;

        for (let i = 0; i < this.game.size; i++) {
            for (let j = 0; j < this.game.size; j++) {
                const cell = this.cells[i][j];
                const value = state.grid[i][j];
                cell.textContent = value || '';
                cell.setAttribute('data-value', value);
                cell.className = 'cell' + (value ? '' : ' empty');
            }
        }
    }

    animateNewTile(tile) {
        if (tile && this.cells[tile.row] && this.cells[tile.row][tile.col]) {
            const cell = this.cells[tile.row][tile.col];
            cell.classList.add('new');
            setTimeout(() => cell.classList.remove('new'), 150);
        }
    }

    showGameOver() {
        if (this.gameOverDisplay && this.finalScoreDisplay) {
            this.gameOverDisplay.classList.add('active');
            this.finalScoreDisplay.textContent = this.game.score;
        }
    }

    hideGameOver() {
        if (this.gameOverDisplay) {
            this.gameOverDisplay.classList.remove('active');
        }
    }
}

// Initialize the game
window.addEventListener('load', () => {
    const gameCore = new GameCore(4);
    const gameUI = new GameUI(gameCore);
    const tournament = new Tournament();
    const tournamentUI = new TournamentUI(tournament);

    // Setup reset button
    const resetButton = document.getElementById('reset-game');
    if (resetButton) {
        resetButton.addEventListener('click', () => {
            gameCore.reset();
            gameUI.hideGameOver();
            gameUI.render();
        });
    }

    // Setup strategy selector
    const strategySelector = document.getElementById('strategy-selector');
    const strategyMap = {
        'clockwise': () => new SequenceStrategy("clockwise"),
        'counter-clockwise': () => new SequenceStrategy("counter-clockwise"),
        'snake': () => new SequenceStrategy("snake"),
        'corner': () => new CornerStrategy()
    };

    // Get saved strategy or use corner as default
    const savedStrategy = localStorage.getItem('selectedStrategy') || 'corner';
    if (strategySelector) {
        strategySelector.value = savedStrategy;
    }
    const createStrategy = strategyMap[savedStrategy] || strategyMap['corner'];
    const strategy = createStrategy();
    gameCore.setStrategy(strategy);
    const autoPlayUI = new AutoPlayUI(gameCore, gameUI, strategy);

    // Handle strategy changes
    if (strategySelector) {
        strategySelector.addEventListener('change', (e) => {
            const createStrategy = strategyMap[e.target.value];
            if (createStrategy) {
                const newStrategy = createStrategy();
                gameCore.setStrategy(newStrategy);
                autoPlayUI.setStrategy(newStrategy);
                localStorage.setItem('selectedStrategy', e.target.value);
            }
        });
    }

    // Handle grid size changes
    const sizeSelector = document.getElementById('size-selector');
    if (sizeSelector) {
        sizeSelector.addEventListener('change', (e) => {
            const size = parseInt(e.target.value);
            const newGameCore = new GameCore(size);
            const currentStrategy = strategyMap[strategySelector.value]();
            newGameCore.setStrategy(currentStrategy);
            const newGameUI = new GameUI(newGameCore);
            autoPlayUI.game = newGameCore;
            autoPlayUI.gameUI = newGameUI;
        });
    }

    // Store instances globally
    window.game = gameCore;
    window.gameUI = gameUI;
    window.tournament = tournament;
}); 