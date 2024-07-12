library(boot)
library(readxl)

file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/13Final.xlsx"
data <- read_excel(file_path, col_names = TRUE)
my_mean_function <- function(data, indices) {return(mean(data[indices]))}

#原始数据分布
quantiles <- quantile(data$mutual_consistency, c(0.025, 0.975))
hist(data$mutual_consistency,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.1), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$mutual_consistency), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$mutual_consistency, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$mutual_consistency))

quantiles <- quantile(data$self_accuracy, c(0.025, 0.975))
hist(data$self_accuracy,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.1), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$self_accuracy), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$self_accuracy, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$self_accuracy))

# 对于MC值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$mutual_consistency, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
# 绘制Bootstrap分布及均值置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
# 绘制2.5%百分位数的线并标注
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
# text(ci$percent[4], par("usr")[3] * 0.95, paste0("2.5%\n", round(ci$percent[4], 2)), col="red", pos=3, adj=0.5)
# 绘制97.5%百分位数的线并标注
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
# text(ci$percent[5], par("usr")[3] * 0.95, paste0("97.5%\n", round(ci$percent[5], 2)), col="red", pos=3, adj=0.5)
# 绘制均值线并标注
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
# text(mean(results$t), par("usr")[3] * 0.95, paste0("Mean\n", round(mean(results$t), 2)), col="blue", pos=1, adj=0.5)
# 计算样本均值的密度曲线
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(results$t, n = 1024)
# 绘制密度曲线
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

# 对于SA值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$self_accuracy, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)


# HC_Simulation
file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/simulation_HC.xlsx"
data <- read_excel(file_path, col_names = TRUE)

quantiles <- quantile(data$health_mutual, c(0.025, 0.975))
hist(data$health_mutual,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$health_mutual), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$health_mutual, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$health_mutual))

quantiles <- quantile(data$health_self, c(0.025, 0.975))
hist(data$health_self,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$health_self), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$health_self, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$health_self))

# HC_Simulation_Bootstrapping
my_mean_function <- function(data, indices) {return(mean(data[indices]))}
set.seed(8023)
results <- boot(data$health_mutual, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) 
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, xlim = c(0.3, 0.9), ylim = c(0, 7))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)






file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/9Positive.xlsx"
data <- read_excel(file_path, col_names = TRUE)
my_mean_function <- function(data, indices) {return(mean(data[indices]))}

#原始数据分布
quantiles <- quantile(data$MC, c(0.025, 0.975))
hist(data$MC,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.1), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$MC), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$MC, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$MC))

quantiles <- quantile(data$SA, c(0.025, 0.975))
hist(data$SA,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.1), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$SA), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$SA, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$SA))

# 对于MC值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$MC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

# 对于SA值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$SA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

# Positive_Simulation
file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/positive_SZ.xlsx"
data <- read_excel(file_path, col_names = TRUE)
quantiles <- quantile(data$positive_mutual, c(0.025, 0.975))
hist(data$positive_mutual,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$positive_mutual), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$positive_mutual, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$positive_mutual))

quantiles <- quantile(data$positive_self, c(0.025, 0.975))
hist(data$positive_self,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 15))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$positive_self), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(data$positive_self, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$positive_self))