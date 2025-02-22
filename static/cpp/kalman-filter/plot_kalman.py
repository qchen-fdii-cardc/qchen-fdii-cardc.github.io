import subprocess
import numpy as np
import matplotlib.pyplot as plt
import os


def compile_kalman():
    """Compile the Kalman filter C program"""
    try:
        result = subprocess.run(
            ['gcc', 'kalman.c', '-o', 'kalman', '-lgsl', '-lgslcblas', '-lm'],
            capture_output=True,
            text=True
        )
        if result.returncode != 0:
            print("Error compiling kalman filter:")
            print(result.stderr)
            return False
        return True
    except Exception as e:
        print(f"Error during compilation: {e}")
        return False


def run_kalman_filter():
    """Run the Kalman filter executable and return the results"""
    try:
        # First compile the program
        if not compile_kalman():
            return None

        # Run the executable and capture output
        result = subprocess.run(['./kalman'], capture_output=True, text=True)

        # Clean up the executable
        try:
            os.remove('./kalman')
        except OSError as e:
            print(f"Warning: Could not remove executable: {e}")

        if result.returncode != 0:
            print("Error running kalman filter executable")
            return None

        # Parse the output
        lines = result.stdout.strip().split('\n')

        # Skip the header line (starts with #)
        data = np.array([line.split()
                        for line in lines if not line.startswith('#')], dtype=float)

        return data
    except Exception as e:
        print(f"Error: {e}")
        # Try to clean up if something went wrong
        try:
            os.remove('./kalman')
        except OSError:
            pass
        return None


def plot_position(data):
    """Plot position tracking results"""
    plt.figure(figsize=(12, 6))

    time = data[:, 0]
    plt.plot(time, data[:, 1], 'b-', label='True Position', linewidth=2)
    plt.scatter(time, data[:, 3], c='r', s=20, alpha=0.5, label='Measurements')
    plt.plot(time, data[:, 4], 'g-', label='Estimated Position', linewidth=2)

    plt.title('Position Tracking')
    plt.xlabel('Time (s)')
    plt.ylabel('Position')
    plt.grid(True)
    plt.legend()

    plt.tight_layout()
    plt.savefig('kalman_filter_position.png', dpi=300, bbox_inches='tight')
    plt.close()


def plot_velocity(data):
    """Plot velocity tracking results"""
    plt.figure(figsize=(12, 6))

    time = data[:, 0]
    plt.plot(time, data[:, 2], 'b-', label='True Velocity', linewidth=2)
    plt.plot(time, data[:, 5], 'g-', label='Estimated Velocity', linewidth=2)

    plt.title('Velocity Tracking')
    plt.xlabel('Time (s)')
    plt.ylabel('Velocity')
    plt.grid(True)
    plt.legend()

    plt.tight_layout()
    plt.savefig('kalman_filter_velocity.png', dpi=300, bbox_inches='tight')
    plt.close()


def plot_results(data):
    """Plot all results"""
    plot_position(data)
    plot_velocity(data)

    # Create a combined plot for display
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))
    fig.suptitle('Kalman Filter Results', fontsize=16)

    # Get time array
    time = data[:, 0]

    # Plot position tracking
    ax1.plot(time, data[:, 1], 'b-', label='True Position', linewidth=2)
    ax1.scatter(time, data[:, 3], c='r', s=20, alpha=0.5, label='Measurements')
    ax1.plot(time, data[:, 4], 'g-', label='Estimated Position', linewidth=2)

    ax1.set_title('Position Tracking')
    ax1.set_xlabel('Time (s)')
    ax1.set_ylabel('Position')
    ax1.grid(True)
    ax1.legend()

    # Plot velocity tracking
    ax2.plot(time, data[:, 2], 'b-', label='True Velocity', linewidth=2)
    ax2.plot(time, data[:, 5], 'g-', label='Estimated Velocity', linewidth=2)

    ax2.set_title('Velocity Tracking')
    ax2.set_xlabel('Time (s)')
    ax2.set_ylabel('Velocity')
    ax2.grid(True)
    ax2.legend()

    # Adjust layout and display
    plt.tight_layout()
    plt.show()


def main():
    # Run Kalman filter and get data
    data = run_kalman_filter()
    if data is not None:
        # Plot results
        plot_results(data)
    else:
        print("Failed to get data from Kalman filter")


if __name__ == "__main__":
    main()
