library(boot)
library(readxl)

file_path <- "C:/Users/ASUS/Desktop/13HC_Trial7_Raw.xlsx"
data <- read_excel(file_path, col_names = TRUE)
my_mean_function <- function(data, indices) {return(mean(data[indices]))}
#原始数据分布
quantiles <- quantile(data$Consistency, c(0.025, 0.975))
hist(data$Consistency,  main = "Distribution of Resampling", 
     xlab = "Consistency", ylab = "Probability Density", 
     freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$Consistency), lty=2, col="blue", lwd=2)
density_curve <- density(data$Consistency, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$Consistency))


quantiles <- quantile(data$Stability, c(0.025, 0.975))
hist(data$Stability,  main = "Distribution of Resampling", 
     xlab = "Stability", ylab = "Probability Density", 
     freq = FALSE, breaks = seq(0.05, 0.2, by=0.01), xlim = c(0.05, 0.2), ylim = c(0, 60))
abline(v = quantiles[1], lty=1, col="red", lwd=2)
abline(v = quantiles[2], lty=1, col="red", lwd=2)
abline(v = mean(data$Stability), lty=2, col="blue", lwd=2)
density_curve <- density(data$Stability, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
cat("95% 置信区间为:", quantiles[1:2], "\n", "均值为：", mean(data$Stability))

set.seed(8023)
results <- boot(data$Consistency, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
hist(results$t,  main = "Distribution of Resampling", # 绘制Bootstrap分布及均值置信区间
    xlab = "Consistency", ylab = "Probability Density", 
    freq = FALSE, breaks = seq(0, 1.0, by=0.05), xlim = c(0, 1.0), ylim = c(0, 7))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
axis(side = 1, at = seq(0.2, 0.8, by=0.2), labels = seq(0.2, 0.8, by=0.2))
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

set.seed(8023)
results <- boot(data$Stability, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为：", mean(results$t)) # 输出置信区间
hist(results$t,  main = "Distribution of Resampling", 
     xlab = "Stability", ylab = "Probability Density", 
     freq = FALSE, breaks = seq(0.05, 0.2, by=0.01), xlim = c(0.05, 0.2), ylim = c(0, 60))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
