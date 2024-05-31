library(boot)
library(readxl)

file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_bootstrapping/6+6(9245).xlsx"
data <- read_excel(file_path, col_names = TRUE)
my_mean_function <- function(data, indices) {return(mean(data[indices]))}
 
# 对于MC值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$mutual_consistency, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为:", mean(results$t), "\n") # 输出置信区间
# 绘制Bootstrap分布及均值置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Mutual Consistency", ylab = "Probability Density", 
    freq = FALSE, xlim = c(0.3, 0.9), ylim = c(0, 7))
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
density_curve <- density(results$t, n = 1024)
# 绘制密度曲线
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

# 对于SA值：执行bootstrap进行1000次重抽样
set.seed(8023)
results <- boot(data$self_accuracy, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") # 使用百分位数法计算CI
cat("95% 置信区间为:", ci$perc[4:5], "\n", "均值为:", mean(results$t), "\n") # 输出置信区间
# 绘制Bootstrap分布及均值置信区间
hist(results$t,  main = "Distribution of Resampling", 
    xlab = "Self Accuracy", ylab = "Probability Density", 
    freq = FALSE, xlim = c(0.3, 0.7), ylim = c(0, 12))
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
density_curve <- density(results$t, n = 1024)
# 绘制密度曲线
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)
