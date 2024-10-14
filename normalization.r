# 原始数据
consistency <- c(0.76, 0.76, 0.54, 0.84, 0.72, 0.80, 0.47, 0.59, 0.90, 0.85, 0.57, 0.62, 0.42, 0.41, 0.60, 0.53, 0.65, 0.47, 0.56, 0.37, 0.32, 0.41, 0.19, 0.34)
stability <- c(0.0908, 0.1300, 0.1198, 0.1732, 0.1296, 0.1374, 0.0925, 0.1401, 0.1263, 0.1256, 0.1342, 0.1451, 0.0944, 0.1013, 0.1031, 0.0899, 0.0997, 0.1473, 0.1294, 0.1431, 0.1127, 0.0943, 0.1231, 0.1289)

# Min-Max 归一化
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
consistency_min_max <- min_max_norm(consistency)
stability_min_max <- min_max_norm(stability)

# 逻辑归一化 (Logistic Normalization)
logistic_norm <- function(x) {
  1 / (1 + exp(-x))
}
consistency_logistic <- logistic_norm(consistency)
stability_logistic <- logistic_norm(stability)

# Robust标准化 (基于中位数和IQR)
robust_norm <- function(x) {
  (x - median(x)) / IQR(x)
}
consistency_robust <- robust_norm(consistency)
stability_robust <- robust_norm(stability)

# SoftMax归一化
softmax_norm <- function(x) {
  exp(x) / sum(exp(x))
}
consistency_softmax <- softmax_norm(consistency)
stability_softmax <- softmax_norm(stability)

# Z-score 标准化结合 Sigmoid 函数
zscore_norm <- function(x) {
  (x - mean(x)) / sd(x)
}
zscore_sigmoid_norm <- function(x) {
  1 / (1 + exp(-zscore_norm(x)))
}
consistency_zscore_sigmoid <- zscore_sigmoid_norm(consistency)
stability_zscore_sigmoid <- zscore_sigmoid_norm(stability)

# Log 归一化 (先取对数再归一化到0-1)
log_norm <- function(x) {
  log_x <- log(x + 1e-9)  # 加上一个小常数避免log(0)
  (log_x - min(log_x)) / (max(log_x) - min(log_x))
}
consistency_log_norm <- log_norm(consistency)
stability_log_norm <- log_norm(stability)

# 输出
cat("原始 Consistency:", paste(consistency, collapse = ", "), "\n")
cat("原始 Stability:", paste(stability, collapse = ", "), "\n")
cat("Min-Max 归一化 Consistency:", paste(consistency_min_max, collapse = ", "), "\n")
cat("Min-Max 归一化 Stability:", paste(stability_min_max, collapse = ", "), "\n")
cat("逻辑归一化 Consistency:", paste(consistency_logistic, collapse = ", "), "\n")
cat("逻辑归一化 Stability:", paste(stability_logistic, collapse = ", "), "\n")
cat("Robust 归一化 Consistency:", paste(consistency_robust, collapse = ", "), "\n")
cat("Robust 归一化 Stability:", paste(stability_robust, collapse = ", "), "\n")
cat("SoftMax 归一化 Consistency:", paste(consistency_softmax, collapse = ", "), "\n")
cat("SoftMax 归一化 Stability:", paste(stability_softmax, collapse = ", "), "\n")
cat("Z-score 归一化结合 Sigmoid Consistency:", paste(consistency_zscore_sigmoid, collapse = ", "), "\n")
cat("Z-score 归一化结合 Sigmoid Stability:", paste(stability_zscore_sigmoid, collapse = ", "), "\n")
cat("Log 归一化 Consistency:", paste(consistency_log_norm, collapse = ", "), "\n")
cat("Log 归一化 Stability:", paste(stability_log_norm, collapse = ", "), "\n")
