library(readxl)
library(writexl)
library(transport)
library(boot)

dataHC <- read_excel("C:/Users/ASUS/Desktop/AI_SCZ/Analysis_ridge/HC_FC.xlsx", col_names = TRUE)
dataHC <- as.data.frame(dataHC)
dataHC[, sapply(dataHC, is.numeric)] <- lapply(dataHC[, sapply(dataHC, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)}  # 检查并中位数替换每一列的缺失值
  return(x)})
dataHC_boot <- matrix(NA, ncol = ncol(dataHC), nrow = 1000)
colnames(dataHC_boot) <- colnames(dataHC)
dataSZ <- read_excel("C:/Users/ASUS/Desktop/AI_SCZ/Analysis_ridge/SZ_FC.xlsx", col_names = TRUE)
dataSZ <- as.data.frame(dataSZ)
dataSZ[, sapply(dataSZ, is.numeric)] <- lapply(dataSZ[, sapply(dataSZ, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- median(x, na.rm = TRUE)}  # 检查并中位数替换每一列的缺失值
  return(x)})
dataSZ_boot <- matrix(NA, ncol = ncol(dataSZ), nrow = 1000)
colnames(dataSZ_boot) <- colnames(dataSZ)
my_mean_function <- function(data, indices) {return(mean(data[indices]))}
for (i in 2:ncol(dataHC)) {
    set.seed(8023)
    results <- boot(dataHC[, i], statistic = my_mean_function, R = 1000)
    dataHC_boot[,i] <- results$t}
for (j in 2:ncol(dataSZ)) {
    set.seed(8023)
    results <- boot(dataSZ[, j], statistic = my_mean_function, R = 1000)
    dataSZ_boot[,j] <- results$t}
W_Distance <- matrix(NA, ncol = ncol(dataHC), nrow = 1)
for (w in 2:ncol(dataHC)) {
    W_Distance[,w] <- wasserstein1d (dataSZ_boot[, w], dataHC_boot[, w], p = 1)}

SZ_FC_processed <- matrix(NA, ncol = ncol(dataSZ), nrow = nrow(dataSZ))
colnames(SZ_FC_processed) <- colnames(dataSZ)
meanSZ <- colMeans(dataSZ, na.rm = TRUE)  # 计算每列的平均值
for (i in 1:nrow(dataSZ)) {
  for (j in 2:ncol(dataSZ)) {
    abs_diff <- abs(dataSZ[i, j] - meanSZ[j])  # 计算差值的绝对值
    SZ_FC_processed[i, j] <- abs_diff * W_Distance[1, j]}}
write_xlsx(as.data.frame(SZ_FC_processed), path = "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_ridge/Results_8samples_raw.xlsx")

