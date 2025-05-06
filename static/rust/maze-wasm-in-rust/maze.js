import init, { Cell, Maze } from "./pkg/hello_wasm.js";

let maze = null;
let canvas = null;
let ctx = null;

init().then(() => {
    // Initialize canvas
    canvas = document.getElementById("canvas");
    ctx = canvas.getContext("2d");
    const progress = document.querySelector('.progress');
    const downloadButton = document.getElementById('downloadMaze');
    const wallColorInput = document.getElementById('wallColor');
    const pathColorInput = document.getElementById('pathColor');
    const startColorInput = document.getElementById('startColor');
    const endColorInput = document.getElementById('endColor');

    // Set up controls
    const sizeInput = document.getElementById("mazeSize");
    const generateButton = document.getElementById("generateMaze");

    // Ensure odd numbers
    function ensureOdd(value) {
        return value % 2 === 0 ? value + 1 : value;
    }

    // Generate new maze
    async function generateNewMaze() {
        const size = ensureOdd(parseInt(sizeInput.value));

        // Update input value to ensure it's odd
        sizeInput.value = size;

        // Show progress and disable buttons
        progress.style.display = 'block';
        generateButton.disabled = true;
        downloadButton.disabled = true;

        // Use setTimeout to allow UI to update
        await new Promise(resolve => setTimeout(resolve, 0));

        try {
            maze = new Maze(size, size);
            renderMaze();
        } finally {
            // Hide progress and enable buttons
            progress.style.display = 'none';
            generateButton.disabled = false;
            downloadButton.disabled = false;
        }
    }

    // Download maze as PNG
    function downloadMaze() {
        if (!maze) return;

        // Create a temporary link
        const link = document.createElement('a');
        link.download = `maze-${maze.get_dimensions()[0]}x${maze.get_dimensions()[1]}.png`;
        link.href = canvas.toDataURL('image/png');
        link.click();
    }

    // Set canvas size to window size
    function resizeCanvas() {
        if (maze) {
            renderMaze();
        }
    }

    // Initial setup
    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);
    generateButton.addEventListener('click', generateNewMaze);
    downloadButton.addEventListener('click', downloadMaze);
    wallColorInput.addEventListener('change', () => maze && renderMaze());
    pathColorInput.addEventListener('change', () => maze && renderMaze());
    startColorInput.addEventListener('change', () => maze && renderMaze());
    endColorInput.addEventListener('change', () => maze && renderMaze());

    // Generate initial maze
    generateNewMaze();

    function renderMaze() {
        // Clear canvas
        ctx.fillStyle = "black";
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        var size = maze.get_dimensions()[0]; // Since width = height, we can use either

        // 固定单元格大小为10像素
        const CELL_SIZE = 10;
        const total_size = size * CELL_SIZE;

        // 设置画布大小
        canvas.width = total_size;
        canvas.height = total_size;

        // Draw background
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, total_size, total_size);

        // Draw start point
        ctx.fillStyle = startColorInput.value;
        ctx.fillRect(
            maze.get_start()[1] * CELL_SIZE,
            maze.get_start()[0] * CELL_SIZE,
            CELL_SIZE,
            CELL_SIZE
        );

        // Draw end point
        ctx.fillStyle = endColorInput.value;
        ctx.fillRect(
            maze.get_end()[1] * CELL_SIZE,
            maze.get_end()[0] * CELL_SIZE,
            CELL_SIZE,
            CELL_SIZE
        );

        // Draw walls
        ctx.fillStyle = wallColorInput.value;
        for (var i = 0; i < size; i++) {
            for (var j = 0; j < size; j++) {
                var cell = maze.get_cell(i, j);
                if (cell === Cell.Wall) {
                    ctx.fillRect(
                        j * CELL_SIZE,
                        i * CELL_SIZE,
                        CELL_SIZE,
                        CELL_SIZE
                    );
                }
            }
        }

        // Draw paths
        ctx.fillStyle = pathColorInput.value;
        for (var i = 0; i < size; i++) {
            for (var j = 0; j < size; j++) {
                var cell = maze.get_cell(i, j);
                if (cell === Cell.Path) {
                    ctx.fillRect(
                        j * CELL_SIZE,
                        i * CELL_SIZE,
                        CELL_SIZE,
                        CELL_SIZE
                    );
                }
            }
        }
    }
});
