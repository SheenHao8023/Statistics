library(readxl)
library(writexl)
library(dplyr)
library(transport)
library(boot)

data = read_excel("C:/Users/ASUS/Desktop/normal.xlsx", col_names = TRUE)
data <- as.data.frame(data)
dataHC = data %>% filter(Group == 'HC')
dataPS = data %>% filter(Group == 'PS')

dataHC[, sapply(dataHC, is.numeric)] <- lapply(dataHC[, sapply(dataHC, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)}  # 检查并中位数替换每一列的缺失值
  return(x)})
dataHC_boot <- matrix(NA, ncol = ncol(dataHC), nrow = 1000)
colnames(dataHC_boot) <- colnames(dataHC)

dataPS[, sapply(dataPS, is.numeric)] <- lapply(dataPS[, sapply(dataPS, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)}  # 检查并中位数替换每一列的缺失值
  return(x)})
dataPS_boot <- matrix(NA, ncol = ncol(dataPS), nrow = 1000)
colnames(dataPS_boot) <- colnames(dataPS)

my_mean_function <- function(data, indices) {return(mean(data[indices]))}
for (i in 2:ncol(dataHC)) {
    set.seed(8023)
    results <- boot(dataHC[, i], statistic = my_mean_function, R = 1000)
    dataHC_boot[,i] <- results$t}
for (j in 2:ncol(dataPS)) {
    set.seed(8023)
    results <- boot(dataPS[, j], statistic = my_mean_function, R = 1000)
    dataPS_boot[,j] <- results$t}
W_Distance <- matrix(NA, ncol = ncol(dataHC), nrow = 1)
for (w in 2:ncol(dataHC)) {
    W_Distance[,w] <- wasserstein1d (dataPS_boot[, w], dataHC_boot[, w], p = 1)}

PS_processed <- matrix(NA, ncol = ncol(dataPS), nrow = nrow(dataPS))
colnames(PS_processed) <- colnames(dataPS)
meanPS <- colMeans(dataPS[sapply(dataPS, is.numeric)], na.rm = TRUE)  # 计算每列的平均值
for (i in 1:nrow(dataPS)) {
  for (j in 2:ncol(dataPS)) {
    abs_diff <- abs(dataPS[i, j] - meanPS[j-1])  # 计算差值的绝对值
    PS_processed[i, j] <- abs_diff * W_Distance[1, j]}}
write_xlsx(as.data.frame(PS_processed), path = "C:/Users/ASUS/Desktop/PS_processed3.xlsx")

