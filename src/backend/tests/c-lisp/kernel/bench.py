import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns # type: ignore
import os
import argparse
from datetime import datetime

parser = argparse.ArgumentParser(
    description=" -k, type of kernel to be benchmarked. cat ./kernels.txt for more info"
)

parser.add_argument(
    "-k",
    type=str,
    default="MMult1",
    help="kernel name"
)

args = parser.parse_args()

os.system("rm tmp/data.csv")
os.system(f"./compile.sh {args.k}")
os.system(f"./build/kernel_bench many | tee tmp/data.csv")

sns.set_style("darkgrid")

df = pd.read_csv("tmp/data.csv", names=["size", "allclose", "ref_gflops", "kernel_gflops"])

sns.lineplot(x="size", y="ref_gflops", data=df, label="Reference GFLOPS")
sns.lineplot(x="size", y="kernel_gflops", data=df, label="Kernel GFLOPS")

plt.legend(title="GFLOPS")
plt.xlabel("Size")
plt.ylabel("GFLOPS")
plt.title(f"Performance of Kernel: {args.k}")

output_dir = "plots"
os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, f"{args.k}_performance_plot_{datetime.now().strftime('%H:%M')}.png")
plt.savefig(output_path)

# plt.show()
