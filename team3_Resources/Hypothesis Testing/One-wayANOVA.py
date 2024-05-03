import scipy.stats as stats
import pandas as pd

data = pd.read_csv('data_anova.csv')


region1 = data.iloc[1:97, 0]
region2 = data.iloc[1:97, 1]
region3 = data.iloc[1:94, 2]

# Hiển thị kết quả
print("Region 1:")
print(region1)
print("\nRegion 2:")
print(region2)
print("\nRegion 3:")
print(region3)

f_statistic, p_value = stats.f_oneway(region1, region2, region3)

alpha = 0.05

print("One-Way ANOVA Results:")
print(f"F-Statistic: {f_statistic:.2f}")
print(f"P-value: {p_value:.15f}")

if p_value < alpha:
    print("Reject the null hypothesis. There is a significant difference between the groups.")
else:
    print("Fail to reject the null hypothesis. There is no significant difference between the groups.")
