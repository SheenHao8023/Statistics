# Load necessary libraries
library(dplyr)
library(MANOVA.RM)
library(emmeans)
library(openxlsx)
# 读取Excel文件
setwd('D:/Research_project/Interperson_FNIRS')
data <- read.xlsx("DegreeRank.xlsx", na.strings = c("", "NA"))

data <- data %>%
  drop_na()
data$centralized_age <- as.numeric(scale(data$age,center = TRUE, scale = FALSE))
data$centralized_age <- data$age-mean(data$age)

#data$Global_Centrality_AllTasks <- as.numeric(scale(rowMeans(data[, c(5:64)], na.rm = TRUE),center = TRUE, scale = FALSE))
#data$Global_Centrality_RS <- as.numeric(scale(rowMeans(data[, c(5:16)], na.rm = TRUE),center = TRUE, scale = FALSE))


# 将数据从宽格式转换为长格式
data_long <- data %>%
  pivot_longer(cols = 5:64, 
               names_to = "Task_BrainRegion", 
               values_to = "DegreeRank")

# 分离任务条件和脑区
data_long$Task <- sub("_.*", "", data_long$Task_BrainRegion)
data_long$BrainRegion <- sub(".*_", "", data_long$Task_BrainRegion)

# 创建新的因子，合并非RS的任务条件为TS
data_long$Task_with_TS <- data_long$Task
data_long$Task_with_TS[data_long$Task != "RS"] <- "TS"
data_long$Task_with_TS <- factor(data_long$Task_with_TS, levels = c("RS", "TS"))

# 将group, Task, BrainRegion, gender 转换为因子
data_long$group <- as.factor(data_long$group)
data_long$Task <- as.factor(data_long$Task)
data_long$Task_with_TS <- as.factor(data_long$Task_with_TS)
data_long$BrainRegion <- as.factor(data_long$BrainRegion)
data_long$gender <- as.factor(data_long$gender)

# 查看整理后的数据
head(data_long)

# 将整理好的数据保存为最终的数据框
#final_data <- data_long[, c("ID", "group", "gender", "centralized_age", "Task", "Task_with_TS","BrainRegion", "Degree", 
#                            "Global_Centrality_AllTasks", "Global_Centrality_RS")]

final_data <- data_long[, c("ID", "group", "gender", "centralized_age", "Task", "Task_with_TS","BrainRegion", "DegreeRank")]
# 保存为Excel文件
write.xlsx(final_data, "DegreeRank_long.xlsx")

# 打印已保存的文件路径
#cat("Final data with centrality metrics has been saved to 'Degree_with_centrality.xlsx'")

# Read the Excel file
file_path <- "D:/Research_project/Interperson_FNIRS/DegreeRank_long.xlsx"
setwd('D:/Research_project/Interperson_FNIRS')
final_data <- read.xlsx(file_path, na.strings = c("", "NA"))
# Converting specified columns to factors
final_data$group <- as.factor(final_data$group)
final_data$gender <- as.factor(final_data$gender)
final_data$Task <- as.factor(final_data$Task)
final_data$Task_with_TS <- as.factor(final_data$Task_with_TS)
final_data$BrainRegion <- as.factor(final_data$BrainRegion)

library(bruceR)     # for MANOVA and post-hoc analysis
library(openxlsx)   # for exporting results to Excel

task_combinations <- list(
  c("RS", "HS", "HEO", "HA", "HB"),
  c("RS", "TS"),
  c("RS", "HEO")
)

# Load the vegan package
library(vegan)
library(cluster)
library(pairwiseAdonis)
library(devtools)
library(officer)

# Define function

perform_permanova <- function(data, covariates, file_name) {
  # Ensure DegreeRank exists in the data
  if (!"DegreeRank" %in% names(data)) {
    stop("DegreeRank variable not found in the dataset.")
  }
  
  # Combine all covariates and factors into a formula for PERMANOVA
  DegreeRank_dist <- vegdist(data$DegreeRank, method = "euclidean")
  
  # Run PERMANOVA
  permanova_results <- adonis2(DegreeRank_dist ~ group * Task * BrainRegion+gender+centralized_age, 
                               data = data, 
                               method = "euclidean", 
                               permutations = 100)
  # Save PERMANOVA results as a CSV
  write.csv(permanova_results, paste0(file_name, "_Permanova_Results.csv"))
  
  # Initialize list to store pairwise results
  pairwise_results <- list()
  # Perform pairwise Mann-Whitney U test for each Task and BrainRegion
  for (task in unique(data$Task)) {
  for (region in unique(data$BrainRegion)) {
    subset_data <- data[data$Task == task & data$BrainRegion == region, ]
    # Perform Mann-Whitney U test (Wilcoxon rank-sum test)
    lm_model<-lm(DegreeRank ~ centralized_age+gender,subset_data) #Regress out covariates
    subset_data$residuals <-residuals(lm_model) #Regress out covariates
    test_result <- wilcox.test(residuals ~ group, data = subset_data, exact = FALSE)
    
    # Store results in a list with task, region, and contrast details
    pairwise_results[[paste(region, sep = "_")]] <- list(
      Contrast = "SCZ vs HC",
      #Task = task,
      BrainRegion = region,
      U_value = test_result$statistic,  # U-statistic
      p_value = test_result$p.value
    )
  }
 }
  # Convert the results list to a data frame
  pairwise_results_df <- do.call(rbind, lapply(pairwise_results, as.data.frame))
  
  # Apply Bonferroni correction
  bonferroni_p_values <- p.adjust(pairwise_results_df$p_value, method = "bonferroni")
  
  # Apply FDR correction
  fdr_p_values <- p.adjust(pairwise_results_df$p_value, method = "fdr")
  
  # Add corrected p-values to the data frame
  pairwise_results_df$Bonferroni_p_value <- bonferroni_p_values
  pairwise_results_df$FDR_p_value <- fdr_p_values
  
  # Save pairwise post-hoc results as a CSV
  write.csv(pairwise_results_df, paste0(file_name, "_Posthoc_Results.csv"))
  
  # View the results
  print("Pairwise comparisons with Bonferroni and FDR corrections:")
  print(pairwise_results_df)
}


# Run the function

  for (task_set in task_combinations[2]) {
    contains_TS <- "TS" %in% task_set
    if (contains_TS) {
      task_data=final_data
      task_data$Task<-task_data$Task_with_TS
    } else {
      task_data <- final_data[final_data$Task %in% task_set, ]
    }
    perform_manova(
      data = task_data, 
      covariates = c("gender", "centralized_age"), 
      file_name = paste(paste(task_set,collapse = "_"),'Rank',sep='_')
    )
  }
