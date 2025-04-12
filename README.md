# missana

## 简介

missana包提供了一套全面的工具，用于分析数据中的缺失值机制(MCAR/MAR/MNAR)。它通过可视化分析、统计检验和相关性分析帮助研究者理解数据中的缺失模式，从而选择最合适的缺失值处理方法。

## 安装

您可以通过GitHub安装missana开发版本：

```r
# 安装devtools包(如果尚未安装)
install.packages("devtools")

# 安装missana
devtools::install_github("xiaoqqjun/missana")


# 主要功能

# analyze_missing_mechanism(): 分析数据中的缺失机制
# handle_missing(): 根据指定方法处理缺失值
# 示例数据
library(missana)

# 使用内置数据集
data(data_test)

# 分析缺失机制
results <- analyze_missing_mechanism(data_test)

# 查看缺失值汇总
print(results$missing_summary)

# 查看高度相关的缺失模式
print(results$high_miss_pairs)

#处理缺失值
# 使用均值插补处理缺失值
data_imputed <- handle_missing(data_test, method = "mean")

# 使用删除法处理缺失值
data_complete <- handle_missing(data_test, method = "deletion")
?analyze_missing_mechanism
?handle_missing
