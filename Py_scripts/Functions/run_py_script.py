import subprocess
import sys

def run_python_script(script_path):
    result = subprocess.run([sys.executable, script_path], capture_output=True, text=True)
    if result.returncode != 0:
        print(f"Error running Python script: {result.stderr}")