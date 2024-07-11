import pandas as pd
import sys
import matplotlib.pyplot as plt

df = pd.read_csv("tmp/x.csv")
# df['kernel_gflops'] = 2 * (df['size'] ** 3) / df['kernel_time'] / 1e9
# df['ref_gflops'] = 2 * (df['size'] ** 3) / df['ref_time'] / 1e9
df = df.fillna(0)

plt.plot(df['size'], df['kernel_gflops'], label='c-lisp')
plt.plot(df['size'], df['ref_gflops'], label='c')
plt.xlabel("size")
plt.ylabel("GFLOPS")
plt.legend()
plt.show()