import subprocess
import sys
import os

sys.path.append(os.path.abspath("Functions"))
from run_py_script import run_python_script
from run_r_script import run_r_script

R_script_directory = "../R_scripts/"
R_scripts = [os.path.abspath(os.path.join(R_script_directory, f)) for f in os.listdir(R_script_directory) if os.path.isfile(os.path.join(R_script_directory, f))]


print("Please choose your release site and feeder locations")
# Step 1: Run the first Python script
run_python_script('Choose_release_coordinates.py')

print("Extracting data from the release site")
print("[............................................................] 0%% complete \n Creating pen shapefiles")
# Step 2: Run the first R script
print("[||||||||||||||||||||||||||||||..............................] 50%% complete \n Extracting data surrounding release pen")
run_r_script(R_scripts[0])
run_r_script(R_scripts[1])
print("[||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||] 100%% complete")

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