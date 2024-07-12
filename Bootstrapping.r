library(boot)
library(readxl)

file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_brain/intra_coh.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)}  # 检查并均值替换每一列的缺失值
  return(x)
})
my_mean_function <- function(data, indices) {return(mean(data[indices]))}

# Figures
set.seed(8023)
results <- boot(data$RA_RDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
cat(ci$perc[4], " ", mean(results$t), " ", ci$perc[4]) # 输出上界、均值、下界，空格分开

hist(results$t,  main = "RA_RM", 
    xlab = "Intra-brain coherence", ylab = "Probability Density", 
    freq = FALSE, xlim = c(0.2, 0.8), ylim = c(0, 15))
abline(v = ci$percent[4], lty=1, col="red", lwd=2)
abline(v = ci$percent[5], lty=1, col="red", lwd=2)
abline(v = mean(results$t), lty=2, col="blue", lwd=2)
density_curve <- density(results$t, n = 1024)
lines(density_curve$x, density_curve$y, col = "black", lwd = 2)

# Tables
for (i in seq_along(data)) {
  data[, i] <- as.numeric(as.character(data[, i]))
}
boot_results <- matrix(NA, nrow = ncol(data), ncol = 3)
column_names <- names(data)
for (i in 1:ncol(data)) {
    set.seed(8023)
    boot_res <- boot(data[ , column_names[i]], statistic = my_mean_function, R = 1000)
    ci <- boot.ci(boot_res, type = "perc", index = 1)
    lower_bound <- ci$perc[4]
    upper_bound <- ci$perc[5]
    mean_boot <- boot_res$t0
    boot_results[i, ] <- c(lower_bound, upper_bound, mean_boot)
}
colnames(boot_results) <- c("Lower_Bound", "Upper_Bound", "Bootstrap_Mean")
boot_df <- as.data.frame(boot_results)
