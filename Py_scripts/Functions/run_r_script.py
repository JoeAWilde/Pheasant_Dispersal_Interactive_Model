import subprocess
import sys


# Function to run an R script
def run_r_script(script_path):
    result = subprocess.run(
        [r"C:\Program Files\R\R-4.4.2\bin\Rscript.exe", script_path],
        capture_output=True,
        text=True)
    
    if result.returncode != 0:
        print(f"Error running R script: {result.stderr}")