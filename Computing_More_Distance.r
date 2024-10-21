library(philentropy)
library(readxl)
library(writexl)
library(transport)

# Bootstrapping值
file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/nonZdata.xlsx"
data <- read_excel(file_path, col_names = FALSE)
data <- as.data.frame(data)

# Squared Euclidean
results5 <- matrix(NA, ncol = 66)
results6 <- matrix(NA, ncol = 66)
for (i in 3:68) {
    se_ij <- squared_euclidean(data[, i], data[, 1], testNA = FALSE)
    results5[1, i-2] <- se_ij  }
for (i in 3:68) {
    se_ij <- squared_euclidean(data[, i], data[, 2], testNA = FALSE)
    results6[1, i-2] <- se_ij  }

# 11个被试概率密度分布值
file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/11Behavior_brain.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)}  # 检查并均值替换每一列的缺失值
  return(x)
})
data <- as.data.frame(data)


# Jensen-Shannon Divergence
results1 <- matrix(NA, ncol = 66)
for (i in 7:72) {
    jsd_ij <- JSD(rbind(data[, i], data[, 5]), unit = "log2", est.prob = "empirical")
    results1[1, i-6] <- jsd_ij  }
results2 <- matrix(NA, ncol = 66)
for (i in 7:72) {
    jsd_ij <- JSD(rbind(data[, i], data[, 6]), unit = "log2", est.prob = "empirical")
    results2[1, i-6] <- jsd_ij  }


# Mutual information
results3 <- matrix(NA, ncol = 66)
results4 <- matrix(NA, ncol = 66)
for (k in 5:72)
    data[, k] <- data[, k] / sum(data[, k])
for (i in 7:72) {
    # 注意：MI函数需要联合概率分布和两个边际概率分布作为输入
    mi_ij <- MI(data[, i], data[, 5], outer(data[, i], data[, 5]))
    results3[1, i-6] <- mi_ij  }
for (i in 7:72) {
    mi_ij <- MI(data[, i], data[, 6], outer(data[, i], data[, 6]))
    results4[1, i-6] <- mi_ij  }

# Wasserstein Distance
# 基于经验分布对象
dataSZ <- read_excel("C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/positive_SZ.xlsx", col_names = TRUE)
dataHC <- read_excel("C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/nonZdata.xlsx", col_names = FALSE)
dataSZ <- as.data.frame(dataSZ)
dataHC <- as.data.frame(dataHC)
MC <- wasserstein1d (dataSZ[, 1], dataHC[, 1], p = 1)
SA <- wasserstein1d (dataSZ[, 2], dataHC[, 2], p = 1)
