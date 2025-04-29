import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation
import os


def read_results(file_path):
    """Reads the results file and extracts time, X[0], and X[1]."""
    time = []
    x0 = []
    x1 = []
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
        for line in file:
            if line.startswith("#") or not line.strip():
                continue
            line = ''.join(char for char in line if char.isprintable())
            parts = line.split()
            if len(parts) >= 3:
                try:
                    time.append(float(parts[0]))
                    x0.append(float(parts[1]))
                    x1.append(float(parts[2]))
                except ValueError:
                    continue
    return np.array(time), np.array(x0), np.array(x1)


def create_animation(time, x0, x1, save_path):
    """Creates an animation of the harmonic oscillator."""
    # Create figure with two subplots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

    # Set up the time series plot
    ax1.set_xlim(time[0], time[-1])
    ax1.set_ylim(-1.2, 1.2)
    ax1.grid(True)
    ax1.set_title('Time Series')
    ax1.set_xlabel('Time')
    ax1.set_ylabel('Value')

    # Set up the phase space plot
    ax2.set_xlim(-1.2, 1.2)
    ax2.set_ylim(-1.2, 1.2)
    ax2.set_aspect('equal')
    ax2.grid(True)
    ax2.set_title('Phase Space')
    ax2.set_xlabel('x1')
    ax2.set_ylabel('x2')

    # Plot the full trajectory in phase space
    ax2.plot(x0, x1, 'b-', alpha=0.3, label='Trajectory')

    # Initialize the time series lines and phase space point
    line1, = ax1.plot([], [], 'b-', label='x1')
    line2, = ax1.plot([], [], 'r-', label='x2')
    phase_point, = ax2.plot([], [], 'ro', markersize=10)

    # Add vertical line for current time
    vline = ax1.axvline(x=0, color='k', linestyle='--', alpha=0.5)

    # Add legends
    ax1.legend()
    ax2.legend()

    def init():
        line1.set_data([], [])
        line2.set_data([], [])
        phase_point.set_data([], [])
        vline.set_xdata([0])
        return line1, line2, phase_point, vline

    def update(frame):
        # Update time series plot
        current_time = time[frame]
        line1.set_data(time[:frame+1], x0[:frame+1])
        line2.set_data(time[:frame+1], x1[:frame+1])
        vline.set_xdata([current_time])

        # Update phase space point
        phase_point.set_data(x0[frame], x1[frame])
        return line1, line2, phase_point, vline

    # Create animation
    anim = FuncAnimation(fig, update, frames=len(time),
                         init_func=init, blit=True,
                         interval=20)  # 50 fps

    # Save animation
    anim.save(save_path, writer='pillow', fps=50)
    print(f"Animation saved to {save_path}")


if __name__ == "__main__":
    # Get the directory of the current script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # Go up one directory to find the results file
    results_dir = os.path.dirname(script_dir)
    results_file = os.path.join(results_dir, "results")

    # Read the results
    time, x0, x1 = read_results(results_file)

    # Create animation
    save_path = os.path.join(results_dir, "harmonic_oscillator.gif")
    create_animation(time, x0, x1, save_path)
