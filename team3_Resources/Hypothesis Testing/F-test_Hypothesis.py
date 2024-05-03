import numpy as np
import scipy.stats as stats
import pandas as pd

data = pd.read_csv('data_month.csv')


region1 = data.iloc[145:286, 2]

region2 = data.iloc[1:145, 2]
region1_numeric = pd.to_numeric(region1, errors='coerce')
region2_numeric = pd.to_numeric(region2, errors='coerce')
# Hiển thị kết quả
print("Region 1:")
print(region1_numeric)

print("\nRegion 2:")
print(region2_numeric)

variance1 = np.var(region1_numeric, ddof=1)  # ddof=1 for sample variance
variance2 = np.var(region2_numeric, ddof=1)

F_statistic = variance1 / variance2

df1 = len(region1_numeric) - 1
df2 = len(region2_numeric) - 1

critical_F = stats.f.ppf(1 - 0.05, df1, df2)

print("F-Test Results:")
print(f"F-Statistic: {F_statistic:.2f}")
print(f"Critical F-Value: {critical_F:.2f}")

if F_statistic > critical_F:
    print("Reject the null hypothesis. Variances are not equal.")
else:
    print("Fail to reject the null hypothesis. Variances are equal.")
