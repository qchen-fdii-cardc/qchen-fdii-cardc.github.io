const canvas = document.getElementById('drawingCanvas');
const ctx = canvas.getContext('2d');
const clearButton = document.getElementById('clear');
const colorPicker = document.getElementById('colorPicker');
const brushSize = document.getElementById('brushSize');
const brushSizeValue = document.getElementById('brushSizeValue');
const exportButton = document.getElementById('export');
const EXPORT_DPI = 300;
const INCH_TO_PIXEL = 96; // Standard screen DPI

let isDrawing = false;
let lastX = 0;
let lastY = 0;

// Initialize canvas
ctx.lineCap = 'round';
ctx.lineJoin = 'round';
ctx.strokeStyle = colorPicker.value;
ctx.lineWidth = brushSize.value;

// Drawing functions
function startDrawing(e) {
    isDrawing = true;
    [lastX, lastY] = [e.offsetX, e.offsetY];
}

function draw(e) {
    if (!isDrawing) return;

    ctx.beginPath();
    ctx.moveTo(lastX, lastY);
    ctx.lineTo(e.offsetX, e.offsetY);
    ctx.stroke();

    [lastX, lastY] = [e.offsetX, e.offsetY];
}

function stopDrawing() {
    isDrawing = false;
}

function exportHighResImage() {
    // Create timestamp for filename
    const now = new Date();
    const year = now.getFullYear();
    const month = String(now.getMonth() + 1).padStart(2, '0');
    const day = String(now.getDate()).padStart(2, '0');
    const time = String(now.getHours()).padStart(2, '0') +
        String(now.getMinutes()).padStart(2, '0') +
        String(now.getSeconds()).padStart(2, '0');
    const filename = `canvas-${year}-${month}${day}-${time}.png`;

    // Create a temporary canvas for high-res export
    const tempCanvas = document.createElement('canvas');
    const tempCtx = tempCanvas.getContext('2d');

    // Calculate size for 300 DPI
    const scale = EXPORT_DPI / INCH_TO_PIXEL;
    tempCanvas.width = canvas.width * scale;
    tempCanvas.height = canvas.height * scale;

    // Scale the context and draw the original canvas
    tempCtx.scale(scale, scale);
    tempCtx.drawImage(canvas, 0, 0);

    // Create download link
    const link = document.createElement('a');
    link.download = filename;
    link.href = tempCanvas.toDataURL('image/png');
    link.click();
}

// Event listeners
canvas.addEventListener('mousedown', startDrawing);
canvas.addEventListener('mousemove', draw);
canvas.addEventListener('mouseup', stopDrawing);
canvas.addEventListener('mouseout', stopDrawing);

clearButton.addEventListener('click', () => {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
});

colorPicker.addEventListener('input', (e) => {
    ctx.strokeStyle = e.target.value;
});

brushSize.addEventListener('input', (e) => {
    ctx.lineWidth = e.target.value;
    brushSizeValue.textContent = e.target.value + 'px';
});

exportButton.addEventListener('click', exportHighResImage);
