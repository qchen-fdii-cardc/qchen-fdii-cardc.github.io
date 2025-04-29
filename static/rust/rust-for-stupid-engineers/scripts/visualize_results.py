import matplotlib.pyplot as plt
import numpy as np
import os


def read_results(file_path):
    """Reads the results file and extracts time, X[0], and X[1]."""
    time = []
    x0 = []
    x1 = []
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
        for line in file:
            if line.startswith("#") or not line.strip():
                continue  # Skip comments and empty lines
            # Clean the line of any non-printable characters
            line = ''.join(char for char in line if char.isprintable())
            parts = line.split()
            if len(parts) >= 3:  # Ensure we have at least 3 values
                try:
                    time.append(float(parts[0]))
                    x0.append(float(parts[1]))
                    x1.append(float(parts[2]))
                except ValueError:
                    continue  # Skip lines that can't be converted to float
    return time, x0, x1


def analytical_solution(t):
    """Returns the analytical solution for the ODE system."""
    # x1(t) = cos(t)
    # x2(t) = -sin(t)
    return np.cos(t), -np.sin(t)


def plot_results(time, x0, x1, save_path=None):
    """Plots X[0] and X[1] against time."""
    plt.figure(figsize=(12, 8))

    # Plot numerical solution
    plt.plot(time, x0, label="X[0] (Numerical)", color="blue", linestyle='-')
    plt.plot(time, x1, label="X[1] (Numerical)", color="red", linestyle='-')

    # Plot analytical solution
    t_analytical = np.linspace(min(time), max(time), 1000)
    x1_analytical, x2_analytical = analytical_solution(t_analytical)
    plt.plot(t_analytical, x1_analytical,
             label="X[0] (Analytical)", color="blue", linestyle='--', alpha=0.5)
    plt.plot(t_analytical, x2_analytical,
             label="X[1] (Analytical)", color="red", linestyle='--', alpha=0.5)

    # Add ODE description
    ode_text = r"$\dot{x_1} = x_2$" + "\n" + \
        r"$\dot{x_2} = -x_1$" + "\n" + "Initial conditions: [1, 0]"
    plt.text(0.02, 0.98, ode_text, transform=plt.gca().transAxes,
             verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))

    plt.title("Harmonic Oscillator: Numerical vs Analytical Solution")
    plt.xlabel("Time")
    plt.ylabel("Values")
    plt.legend()
    plt.grid(True)

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"Plot saved to {save_path}")
    else:
        plt.show()


if __name__ == "__main__":
    # Get the directory of the current script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # Go up one directory to find the results file
    results_dir = os.path.dirname(script_dir)
    results_file = os.path.join(results_dir, "results")
    time, x0, x1 = read_results(results_file)
    plot_results(time, x0, x1, save_path=os.path.join(
        results_dir, "results_plot.png"))
