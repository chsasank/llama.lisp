import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns  # type: ignore
import os
import argparse
from datetime import datetime
import subprocess

"""
Kernel Benchmarking Script
This script benchmarks various kernels written in C and Lisp.
It compiles them to LLVM IR, generates object files, executes the benchmarks,
and records performance metrics. The results are visualized using matplotlib and seaborn.

Requirements:
- Python 3.x
- pandas: For handling CSV data.
- matplotlib: For plotting graphs.
- seaborn: For improved plotting aesthetics.
- Guile: For processing .sexp files.
- Clang: For compiling C code.
- Linux/MacOS: Designed to run in a Unix-like environment.

Usage:
To run the benchmarking script, use the command line to specify the kernel you want to benchmark
along with any desired options:
python benchmark.py -k <kernel_name> -m <mode> -p <print_flag>

Command-Line Arguments:
-k (string): Name of the kernel to benchmark. Default is "MMult1". Refer to kernels.txt for options.
-m (string): Mode of execution. Default is "once".
-p (int): Print resultant matrix flag. Default is 0.
"""

parser = argparse.ArgumentParser(
    description=" -k, type of kernel to be benchmarked. cat ./kernels.txt for more info"
)

parser.add_argument("-k", type=str, default="MMult1", help="kernel name")
parser.add_argument("-m", type=str, default="once", help="Mode")
parser.add_argument("-p", type=int, default=0, help="Print resultant matrix")

args = parser.parse_args()

BUILD_DIRECTORY = "build"
C_KERNELS_DIRECTORY = "kernels_C"
C_LISP_KERNELS_DIRECTORY = "kernels_c-lisp"
RUNTIME_DIRECTORY = "runtime"
TMP = "tmp"
PLOTS = "plots"

CC = "clang"
OPTIMIZATION = "-O1"
FLAGS = "-Wno-implicit-function-declaration -Wno-override-module"

# Create directories if they don't exist
os.makedirs(BUILD_DIRECTORY, exist_ok=True)
os.makedirs(f"{BUILD_DIRECTORY}/ir", exist_ok=True)
os.makedirs(f"{BUILD_DIRECTORY}/obj", exist_ok=True)
os.makedirs(TMP, exist_ok=True)
os.makedirs(PLOTS, exist_ok=True)

# Clean the obj directory
for file in os.listdir(f"{BUILD_DIRECTORY}/obj"):
    file_path = os.path.join(f"{BUILD_DIRECTORY}/obj", file)
    if os.path.isfile(file_path):
        os.unlink(file_path)

# Clean the ir directory
for file in os.listdir(f"{BUILD_DIRECTORY}/ir"):
    file_path = os.path.join(f"{BUILD_DIRECTORY}/ir", file)
    if os.path.isfile(file_path):
        os.unlink(file_path)

# Compile runtime and both kernels
C_kernel = f"{C_KERNELS_DIRECTORY}/{args.k}.c"
c_lisp_kernel = f"{C_LISP_KERNELS_DIRECTORY}/{args.k}.sexp"
macro_file = f"{C_LISP_KERNELS_DIRECTORY}.{args.k}.py"
ir_file = f"{BUILD_DIRECTORY}/ir/{args.k}.ll"
print(ir_file)
C_obj = f"{BUILD_DIRECTORY}/obj/{args.k}_c.o"
c_lisp_obj = f"{BUILD_DIRECTORY}/obj/{args.k}_c-lisp.o"
executable = f"{BUILD_DIRECTORY}/kernel_bench"

# Compile the .sexp kernel
cmd = (
    f"guile ../../../utils/sexp-json.scm < {c_lisp_kernel} | "
    f"python ../../../prelisp.py {macro_file} |"
    f"python ../../../c-lisp.py | "
    f"python ../../../brilisp.py | "
    f"python ../../../llvm.py > {ir_file}"
)
print(f"Executing command to generate LLVM IR:\n{cmd}")
subprocess.run(cmd, shell=True, check=True)

if os.path.exists(ir_file):
    print(f"Successfully created {ir_file}")
    if not os.path.exists(c_lisp_obj):
        compile_cmd = f"{CC} {OPTIMIZATION} {FLAGS} -c -o {c_lisp_obj} {ir_file}"
        print(f"Compiling LLVM IR to object file:\n{compile_cmd}")
        subprocess.run(compile_cmd, shell=True, check=True)
else:
    print(f"Failed to create {ir_file}")

# Compile the C kernel
if not os.path.exists(C_obj):
    compile_cmd = f"{CC} {OPTIMIZATION} {FLAGS} -c -o {C_obj} {C_kernel}"
    print(f"Compiling C kernel to object file:\n{compile_cmd}")
    subprocess.run(compile_cmd, shell=True, check=True)

# Compile main.c and matrix.c
main_file = f"{RUNTIME_DIRECTORY}/main.c"
main_object = f"{BUILD_DIRECTORY}/obj/main.o"
matrix_file = f"{RUNTIME_DIRECTORY}/matrix.c"
matrix_object = f"{BUILD_DIRECTORY}/obj/matrix.o"

compile_cmd = f"{CC} {OPTIMIZATION} {FLAGS} -c -o {main_object} {main_file}"
print(f"Compiling main.c to object file:\n{compile_cmd}")
subprocess.run(compile_cmd, shell=True, check=True)

compile_cmd = f"{CC} {OPTIMIZATION} {FLAGS} -c -o {matrix_object} {matrix_file}"
print(f"Compiling matrix.c to object file:\n{compile_cmd}")
subprocess.run(compile_cmd, shell=True, check=True)

# Link all objects
objects = " ".join(
    map(lambda x: f"{BUILD_DIRECTORY}/obj/{x}", os.listdir(f"{BUILD_DIRECTORY}/obj"))
)
link_cmd = f"{CC} {OPTIMIZATION} {FLAGS} -o {executable} {objects}"
print(f"Linking object files to create executable:\n{link_cmd}")
subprocess.run(link_cmd, shell=True, check=True)

# Execute the executable and store output
now = datetime.now()
formatted_time = now.strftime("%Y:%m:%d:%H:%M:%S")
output_csv = f"{TMP}/{args.k}_{formatted_time}.csv"
execution_cmd = f"{executable} {args.m} {args.p} | tee {output_csv}"
print(f"Executing kernel benchmark:\n{execution_cmd}")

try:
    subprocess.run(execution_cmd, shell=True, check=True)

    if args.m == "many":
        names = ["size", "allclose", "ref_gflops", "kernel_gflops"]
        print(f"Reading output CSV file: {output_csv}")
        df = pd.read_csv(output_csv, names=names, header=None)

        # Plotting the results
        sns.set_style("darkgrid")
        sns.lineplot(x="size", y="ref_gflops", data=df, label="Reference GFLOPS")
        sns.lineplot(x="size", y="kernel_gflops", data=df, label="Kernel GFLOPS")
        plt.legend(title="GFLOPS")
        plt.xlabel("Size")
        plt.ylabel("GFLOPS")
        plt.ylim(0, 10)
        plt.title(f"Performance of Kernel: {args.k}")
        save_path = f"{PLOTS}/{args.k}_{formatted_time}.png"
        print(f"Saving plot to: {save_path}")
        plt.savefig(save_path)
        plt.close()

    else:
        os.system(f"rm {output_csv}")

except KeyboardInterrupt:
    print("\nBenchmarking interrupted")
    os.system(f"rm {output_csv}")
    exit()
