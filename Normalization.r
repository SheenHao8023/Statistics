# 原始数据
consistency <- c(0.758723689,0.741630831,0.536109874,0.843714961,0.702517468,0.775710504,0.459860625,0.561674837,0.897711598,0.850828482,0.540000902,0.660758868,
                 0.360305699,0.581103409,0.544427806,0.667384939,0.413428139,0.446378113,0.550374351,0.364488531,0.316866413,0.426448143,0.191215952,0.344406859)
stability <- c(0.091172409,0.133698286,0.1211534,0.175871764,0.129615282,0.134330704,0.094701142,0.140119631,0.12641324,0.128498997,0.133934345,0.144711743,
               0.099161739,0.107431095,0.089916282,0.097764815,0.087530843,0.148814035,0.128764674,0.1415967,0.114875641,0.097051116,0.125112163,0.129765997)

# Z-score 标准化结合 Sigmoid 函数
zscore_norm <- function(x) {
  (x - mean(x)) / sd(x)
}
zscore_sigmoid_norm <- function(x) {
  1 / (1 + exp(-zscore_norm(x)))
}
consistency_zscore_sigmoid <- zscore_sigmoid_norm(consistency)
stability_zscore_sigmoid <- zscore_sigmoid_norm(stability)
cat("Z-score 归一化结合 Sigmoid Consistency:", paste(consistency_zscore_sigmoid, collapse = ", "), "\n")
cat("Z-score 归一化结合 Sigmoid Stability:", paste(stability_zscore_sigmoid, collapse = ", "), "\n")

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
cat("Log 归一化 Consistency:", paste(consistency_log_norm, collapse = ", "), "\n")
cat("Log 归一化 Stability:", paste(stability_log_norm, collapse = ", "), "\n")
