import csv
from pathlib import Path
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
DATA_CSV = ROOT / "data.csv"
OUT_DIR = ROOT / "imgs"
OUT_DIR.mkdir(parents=True, exist_ok=True)
OUT_FILE = OUT_DIR / "data_plot.png"

xs = []
ys = []
with DATA_CSV.open() as f:
    reader = csv.reader(f)
    for row in reader:
        if not row:
            continue
        try:
            x = float(row[0])
            y = float(row[1])
        except Exception:
            continue
        xs.append(x)
        ys.append(y)

plt.figure(figsize=(8, 5))
plt.plot(xs, ys, marker='o', linestyle='-', color='#1f77b4')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Data Plot from data.csv')
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig(OUT_FILE, dpi=300)
print(f"Saved plot to: {OUT_FILE}")
