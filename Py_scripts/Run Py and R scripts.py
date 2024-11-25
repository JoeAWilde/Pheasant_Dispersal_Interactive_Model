import subprocess
import sys
import os
from tqdm import tqdm
import time

# Function to run a Python script
def run_python_script(script_path):
    result = subprocess.run([sys.executable, script_path], capture_output=True, text=True)
    if result.returncode != 0:
        print(f"Error running Python script: {result.stderr}")
    else:
        print(f"Output from Python script: {result.stdout}")

# Function to run an R script
def run_r_script(script_path):
    result = subprocess.run(
        [r"C:\Program Files\R\R-4.4.1\bin\Rscript.exe", script_path],
        text=True, 
        stdout = sys.stdout, 
        stderr= sys.stderr)
    
    if result.returncode != 0:
        print(f"Error running R script: {result.stderr}")
"""     else:
        print(f"Output from R script: {result.stdout}") """

def run_r_script_with_progress(script_path):
    # Start the R script process
    process = subprocess.Popen(
        [r"C:\Program Files\R\R-4.4.1\bin\Rscript.exe", script_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    # Initialize the progress bar
    progress = None
    total_steps = None

    for line in process.stdout:
        # Print the raw output for debugging, if needed
        print(line.strip())

        # Detect the total steps from the R script (optional if always fixed)
        if "total_steps" in line and total_steps is None:
            total_steps = int(line.split("=")[1].strip())
            progress = tqdm(total=total_steps, desc="Running R script", unit="step")

        # Look for progress updates
        if "PROGRESS:" in line:
            current_step = int(line.split(":")[1].split("/")[0].strip())
            if progress is not None:
                progress.n = current_step
                progress.refresh()

    # Wait for the process to finish
    process.wait()

    # Close the progress bar
    if progress:
        progress.close()

    # Check for errors
    if process.returncode != 0:
        print(f"Error: {process.stderr.read()}")

print("Please choose your release site and feeder locations")
# Step 1: Run the first Python script
run_python_script('Choose_release_coordinates.py')

R_script_directory = "../R_scripts/"
R_scripts = [os.path.abspath(os.path.join(R_script_directory, f)) for f in os.listdir(R_script_directory) if os.path.isfile(os.path.join(R_script_directory, f))]

print("Extracting data from the release site")
# Step 2: Run the first R script
run_r_script_with_progress(R_scripts[0])
run_r_script_with_progress(R_scripts[1])
print("DONE.")

# Step 3: Ask for user input
user_input = input("Would you like to edit the habitat at the release site? (yes/no): ").strip().lower()

# Step 4: Conditional execution based on user input
if user_input == 'yes':
    # Run another Python script based on the user's response
    run_python_script('Edit_habitat_raster.py')

""" # Step 5: Get more user input
more_input = input("Please provide some input for further processing: ")

# Step 6: Run additional R scripts based on the input
run_r_script('second_script.R')

# Run another R script
run_r_script('third_script.R')

# Step 7: Display plot using matplotlib (or any other Python plotting library)
import matplotlib.pyplot as plt

# Example: Display a simple plot
plt.plot([1, 2, 3, 4], [1, 4, 9, 16])
plt.title("Sample Plot")
plt.xlabel("X Axis")
plt.ylabel("Y Axis")
plt.show()

 """