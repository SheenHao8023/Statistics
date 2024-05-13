# R packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(purrr)
library(mmrm)
library(bruceR)

#################################################################
############ Data preparation and preprocessing #################
#################################################################

# Read the raw data and extract the specified columns
file_path <- "C:/Users/ASUS/Desktop/Graduation_Project/data/Behavior_Results.xlsx"
mydata <- read_excel(file_path, col_names = TRUE)
selected_columns <- c("ExperimentName", "Subject", "Age", "Group", "Handedness", "Sex", "Trial", "cijileixing", "cijiwu.ACC", "cijiwu.RT")  # Based on the raw data
mydata_selected <- mydata[selected_columns]

# Rename "Subject" column to ensure sequential stability
mydata_selected$Subject <- ifelse(nchar(mydata_selected$Subject) == 2, paste0("0", mydata_selected$Subject), mydata_selected$Subject)
mydata_selected$Subject <- as.character(mydata_selected$Subject)

# Define stimulus group: 1 = Anodal & 2 = Sham
Subjects_to_replace <- c("012", "022", "032", "041", "051", "061", "072", "081", "092", "101", "111", "141")  # Vector of anodal stimulus group
mydata_selected$Group <- ifelse(mydata_selected$Subject %in% Subjects_to_replace, 1, 2)

# Randomly replaces "cijiwu.RT" with a value of 0
mydata_filled <- mydata_selected %>%
  mutate(cijiwu.RT = ifelse(cijiwu.RT == 0, runif(n(), min = 2000, max = 6000), cijiwu.RT))

# Write preprocessed data
write_xlsx(mydata_filled, "C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/mydata_filled.xlsx")

#################################################################
############# Construct the data analysis file ##################
#################################################################

# Demographic variables and groups
columns_to_extract <- c("Subject", "Age", "Group", "Handedness", "Sex")
processed_subjects <- c()
subject_data_list <- list()

for (i in which(mydata_filled$ExperimentName == "?????  10min(???)")) { # Based on the raw data
  subject_value <- mydata_filled[i, "Subject"]
  if (!(subject_value %in% processed_subjects)) {
    subject_data <- mydata_filled[i, columns_to_extract]
    subject_data_list[[length(subject_data_list) + 1]] <- subject_data
    processed_subjects <- c(processed_subjects, subject_value)
  }
}  # Demographic and group information is extracted for each subject

demo_information<- do.call(rbind, subject_data_list) %>%
    arrange(Subject)
repeated_demo_information <- do.call(rbind, replicate(5, demo_information, simplify = FALSE)) # 5 lines are generated for each subject

# Calculate indices of Signal Detection Theory (SDT) 
filtered_data <- mydata_filled %>% 
  group_by(Subject) %>%
  filter(!abs(cijiwu.RT - mean(cijiwu.RT)) > 3 * sd(cijiwu.RT)) # +/-3SD for each subject is eliminated based on the RTs

Blocks <- c("?????  10min(???)", "???????", "???????", "???????", "???????") # Based on the raw data
Trials <- list(c(1, 2), c(1, 2), c(3, 4), c(5, 6), c(7, 8) ) # Based on the raw data
# Define the function for SDT indices
calculate_sdt <- function(block, trials) {
  filtered_data %>%
    filter(ExperimentName == block & Trial %in% trials) %>%
    group_by(Subject) %>%
    summarise(
        hits = sum(cijileixing == "M" & cijiwu.ACC == 1),  # There is a target and judged to be yes, the response is correct.
        fas = sum(cijileixing == "N" & cijiwu.ACC == 0),  # There is no target but judged to be yes, the response is wrong.
        misses = sum(cijileixing == "M" & cijiwu.ACC == 0),  # There is a target but judged to be no, the response is wrong.
        rejections = sum(cijileixing == "N" & cijiwu.ACC == 1),  # There is no target and judged to be no, the response is correct.
        corrections = sum(cijiwu.ACC == 1), 
        HR = hits / (hits + misses),  # hit rate = hits / (hits + misses)
        FAR = fas / (fas + rejections),  # false alarm rate = false alarms / (false alarms + correct rejections)
        ACC = corrections / (hits + misses + fas + rejections),  
        d_prime = qnorm(HR) - qnorm(FAR),
        beta = HR / FAR
    )
}
results_list <- map2(.x = Blocks, .y = Trials, ~ calculate_sdt(.x, .y))  # 5 blocks are calculated separately
results_df <- bind_rows(results_list) 

# Write demographic and SDT indices
results_df <- results_df[, -1]  # remove the 1st column(Subject)
long_vector <- c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
Time_vector <- matrix(long_vector, nrow = 120, ncol = 1, byrow = TRUE) # generate the time vector (visit)
mydata_sdt_mmrm <- cbind(repeated_demo_information, Time_vector, results_df)
mydata_sdt_mmrm$Time_vector <- factor(mydata_sdt_mmrm$Time_vector)  # Convert data type
mydata_sdt_mmrm[mydata_sdt_mmrm$d_prime == Inf, "d_prime"] <- NA
mydata_sdt_mmrm[mydata_sdt_mmrm$beta == Inf, "beta"] <- NA  # Fill default value
write_xlsx(mydata_sdt_mmrm, "C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/mydata_sdt_mmrm.xlsx")

#################################################################
############# Mixed models for repeated measures#################
#################################################################

sink("C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/mmrm_model_summaries.txt")

# Mixed-effects Model for Repeated Measures (MMRM) with two factors
fit_d_prime <- mmrm(
  formula = d_prime ~ Group + Time_vector + Group * Time_vector + us(Time_vector | Subject) + Age + Handedness + Sex,
  data = mydata_sdt_mmrm,
  reml = TRUE,
  method = "Kenward-Roger"
)  #It is calculated by default even if main factors are not written.
summary(fit_d_prime)
cat("\n\n")

fit_beta <- mmrm(
  formula = beta ~ Group + Time_vector + Group * Time_vector + us(Time_vector | Subject) + Age + Handedness + Sex,
  data = mydata_sdt_mmrm,
  reml = TRUE,
  method = "Kenward-Roger"
)  
summary(fit_beta)
cat("\n\n")

fit_HR <- mmrm(
  formula = HR ~ Group + Time_vector + Group * Time_vector + us(Time_vector | Subject) + Age + Handedness + Sex,
  data = mydata_sdt_mmrm,
  reml = TRUE,
  method = "Kenward-Roger"
)  
summary(fit_HR)
cat("\n\n")

fit_FAR <- mmrm(
  formula = FAR ~ Group + Time_vector + Group * Time_vector + us(Time_vector | Subject) + Age + Handedness + Sex,
  data = mydata_sdt_mmrm,
  reml = TRUE,
  method = "Kenward-Roger"
)  
summary(fit_FAR)
cat("\n\n")

fit_ACC <- mmrm(
  formula = ACC ~ Group + Time_vector + Group * Time_vector + us(Time_vector | Subject) + Age + Handedness + Sex,
  data = mydata_sdt_mmrm,
  reml = TRUE,
  method = "Kenward-Roger"
)  
summary(fit_ACC)

sink()

#################################################################
################### Repeated measures ANOVA #####################
#################################################################

# Construct the new data file
results_indices <- results_df[c("HR", "FAR", "ACC", "d_prime", "beta")]
small_matrices <- vector("list", 5)
for (i in 1:5) {
    start_index <- (i - 1) * 24 + 1
    end_index <- min(i * 24, 120)
    small_matrices[[i]] <- results_indices[start_index:end_index, ]
}
matrix1 <- small_matrices[[1]]
matrix2 <- small_matrices[[2]]
matrix3 <- small_matrices[[3]]
matrix4 <- small_matrices[[4]]
matrix5 <- small_matrices[[5]]
combined_matrix <- cbind(matrix1, matrix2, matrix3, matrix4, matrix5)
new_column_names <- c("HR1", "FAR1", "ACC1", "dprime1", "beta1", "HR2", "FAR2", "ACC2", "dprime2", "beta2",
                      "HR3", "FAR3", "ACC3", "dprime3", "beta3", "HR4", "FAR4", "ACC4", "dprime4", "beta4", "HR5", "FAR5", "ACC5", "dprime5", "beta5")
names(combined_matrix) <- new_column_names
for (col in names(combined_matrix)) {
  combined_matrix[[col]][is.infinite(combined_matrix[[col]])] <- NA
}  # Fill default value
mydata_sdt_rmanova <- cbind(demo_information, combined_matrix)
write_xlsx(mydata_sdt_rmanova, "C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/mydata_sdt_rmanova.xlsx")
# Whether the data format is long or wide data does not affect the analysis in R (bruceR package). 
# The purpose of converting to wide data here is to check easily or export to other software, such as SPSS or JASP, etc.

#Descriptive statistics
Describe(
  mydata_sdt_rmanova,
  all.as.numeric = FALSE,
  digits = 2,
  file = "C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/Descriptive.docx",
)

sink("C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/rmanova_summaries.txt")

MANOVA(
  mydata_sdt_rmanova,
  dvs = c("HR1", "HR2", "HR3", "HR4", "HR5"),
  dvs.pattern = "HR(.)",
  between = "Group",
  within = "Block",
  covariate = c("Sex", "Age", "Handedness"),
  ss.type = "III",
  sph.correction = "GG",
  aov.include = FALSE,
  digits = 3,
  file = NULL
) ->rmanova_HR
summary(rmanova_HR)
cat("\n\n")

MANOVA(
  mydata_sdt_rmanova,
  dvs = c("FAR1", "FAR2", "FAR3", "FAR4", "FAR5"),
  dvs.pattern = "FAR(.)",
  between = "Group",
  within = "Block",
  covariate = c("Sex", "Age", "Handedness"),
  ss.type = "III",
  sph.correction = "GG",
  aov.include = FALSE,
  digits = 3,
  file = NULL
) ->rmanova_FAR
summary(rmanova_FAR)
cat("\n\n")

MANOVA(
  mydata_sdt_rmanova,
  dvs = c("ACC1", "ACC2", "ACC3", "ACC4", "ACC5"),
  dvs.pattern = "ACC(.)",
  between = "Group",
  within = "Block",
  covariate = c("Sex", "Age", "Handedness"),
  ss.type = "III",
  sph.correction = "GG",
  aov.include = FALSE,
  digits = 3,
  file = NULL
) ->rmanova_ACC
summary(rmanova_ACC)
cat("\n\n")

MANOVA(
  mydata_sdt_rmanova,
  dvs = c("dprime1", "dprime2", "dprime3", "dprime4", "dprime5"),
  dvs.pattern = "dprime(.)",
  between = "Group",
  within = "Block",
  covariate = c("Sex", "Age", "Handedness"),
  ss.type = "III",
  sph.correction = "GG",
  aov.include = FALSE,
  digits = 3,
  file = NULL
) ->rmanova_dprime
summary(rmanova_dprime)
cat("\n\n")

MANOVA(
  mydata_sdt_rmanova,
  dvs = c("beta1", "beta2", "beta3", "beta4", "beta5"),
  dvs.pattern = "beta(.)",
  between = "Group",
  within = "Block",
  covariate = c("Sex", "Age", "Handedness"),
  ss.type = "III",
  sph.correction = "GG",
  aov.include = FALSE,
  digits = 3,
  file = NULL
) ->rmanova_beta
summary(rmanova_beta)

sink()

#################################################################
################ Statistical visualization ######################
#################################################################
library(readxl)
mydata <- read_excel("C:/Users/ASUS/Desktop/Graduation_Project/data/Processing/mydata_sdt_mmrm.xlsx")
mydata$Group <- as.factor(mydata$Group)
library(ggplot2)
library(ggprism)
library(RColorBrewer)
# HR 小提琴图与箱线图
ggplot(data = mydata, aes(x = Time_vector, y = HR)) + 
  ylim(0, 1) +   # 设定y轴范围
  xlab("Block") +  # 设置x轴名称
  ylab("Hit rate") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑
  geom_boxplot(aes(fill = Group), width=0.3,
               position = position_dodge(0.5),  outlier.color = NA, whisker.width = 0)+
  # 添加统计摘要层以显示平均数
  stat_summary(aes(group = Group), fun.y = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.5)) +
  # stat_summary(aes(group = Group), fun.y = median, geom = "crossbar", width = 0.1, position = position_dodge(0.5)) +
  scale_fill_manual(name = "Legend Title", values = c("#EE7D80","#D3D3D3"), # 这里需要指定所有使用的颜色
                    labels = c("Anodal", "Sham")) + # 对应的颜色标签
  scale_x_discrete(labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism() + 
  theme(axis.title.x = element_text(size = 14),  # 改变x轴标题的字体大小
        axis.text.x = element_text(size = 12))   # 改变x轴标签的字体大小

#FAR
ggplot(data = mydata, aes(x = Time_vector, y = FAR)) +  ylim(0, 0.5) +   # 设定y轴范围为-5到5
      xlab("Block") +  # 设置x轴名称
      ylab("False alarm rate") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑)
  geom_boxplot(aes(fill = Group), width=0.25,
               position = position_dodge(0.5),  outlier.color = NA)+
  scale_fill_manual(name = "Legend Title", values = c("#EE7D80","#D3D3D3","#D3D3D3", "#EE7D80"), # 这里需要指定所有使用的颜色
                    labels = c("Anodal", "Sham", "Sham","Anodal")) + # 对应的颜色标签
  scale_x_discrete(labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism()

# 按组计算Z分数
mydata <- mydata %>%
  group_by(Group) %>%
  mutate(Zbeta = scale(beta))
#beta
ggplot(data = mydata, aes(x = Time_vector, y = Zbeta)) +  ylim(-1, 2) +   # 设定y轴范围为-5到5
      xlab("Block") +  # 设置x轴名称
      ylab("β(Z-score)") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑)
  geom_boxplot(aes(fill = Group), width=0.25,
               position = position_dodge(0.5),  outlier.color = NA)+
  scale_fill_manual(name = "Legend Title", values = c("#EE7D80","#D3D3D3","#D3D3D3", "#EE7D80"), # 这里需要指定所有使用的颜色
                    labels = c("Anodal", "Sham", "Sham","Anodal")) + # 对应的颜色标签
  scale_x_discrete(labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism()

#dprime
ggplot(data = mydata, aes(x = Time_vector, y = d_prime)) +  ylim(0, 0.5) +   # 设定y轴范围为-5到5
      xlab("Block") +  # 设置x轴名称
      ylab("d'") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑)
  geom_boxplot(aes(fill = Group), width=0.25,
               position = position_dodge(0.5),  outlier.color = NA)+
  scale_fill_manual(name = "Legend Title", values = c("#EE7D80","#D3D3D3","#D3D3D3", "#EE7D80"), # 这里需要指定所有使用的颜色
                    labels = c("Anodal", "Sham", "Sham","Anodal")) + # 对应的颜色标签
  scale_x_discrete(labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism()