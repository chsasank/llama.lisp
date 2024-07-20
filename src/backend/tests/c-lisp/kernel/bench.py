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

parser.add_argument(
    "-m",
    type=str,
    default="once",
    help="Mode"
)

parser.add_argument(
    "-p",
    type=int,
    default=0,
    help="Print resultant matrix"
)

parser.add_argument(
    "-pl",
    type=int,
    default=0,
    help="Plot the results"
)

args = parser.parse_args()

os.system("rm tmp/data.csv")
os.system(f"./compile.sh {args.k}")
os.system(f"./build/kernel_bench {args.m} {args.p}| tee tmp/data_{args.k}_{args.m}.csv")


if args.pl == 1:
    df = pd.read_csv(f"tmp/data_{args.k}_{args.m}.csv", names=["size", "allclose", "ref_gflops", "kernel_gflops"])
    
    sns.set_style("darkgrid")
    sns.lineplot(x="size", y="ref_gflops", data=df, label="Reference GFLOPS")
    sns.lineplot(x="size", y="kernel_gflops", data=df, label="Kernel GFLOPS")

    plt.legend(title="GFLOPS")
    plt.xlabel("Size")
    plt.ylabel("GFLOPS")
    plt.ylim(0, 10)
    plt.title(f"Performance of Kernel: {args.k}")

    output_dir = "plots"
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, f"{args.k}_performance_plot_{datetime.now().strftime('%H:%M')}.png")
    plt.savefig(output_path)

