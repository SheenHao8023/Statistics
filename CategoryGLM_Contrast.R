# 写循环之前的本质过程
# em_prior_ic <- emtrends(IC_model1, var = "Prior_IC", specs = ~ Group)
# contrast_prior_ic <- contrast(em_prior_ic, method = list("PS vs NS" = c(0, -1, 1)))
# em_prior_ws <- emtrends(IC_model1, var = "Prior_WS", specs = ~ Group)
# contrast_prior_ws <- contrast(em_prior_ws, method = list("PS vs NS" = c(0, -1, 1)))
# summary(contrast_prior_ic)
# summary(contrast_prior_ws)

library(readr)
library(bruceR)
library(emmeans)

# Load data
nd_data <- read_csv("C:/Users/ASUS/Desktop/nd_sim_21nov.csv")
sub1data <- subset(nd_data, Group %in% c('HC', 'PS', 'NS'))
sub1data$Group <- factor(sub1data$Group, levels = c('HC', 'PS', 'NS'))
sub2data <- subset(nd_data, Group %in% c('HC', 'PS', 'BA', 'WD'))
sub2data$Group <- factor(sub2data$Group, levels = c('HC', 'PS', 'BA', 'WD'))

# 创建模型
create_models <- function(data) {
  IC_model <- glm(IC ~ Group * (Prior_IC + Prior_WS + IC_pcs + WS_pcs + Pr_IC + Pr_WS), 
                  data = data, family = gaussian())
  WS_model <- glm(WS ~ Group * (Prior_IC + Prior_WS + IC_pcs + WS_pcs + Pr_IC + Pr_WS),
                  data = data, family = gaussian())
  model_summary(list(IC_model, WS_model))
  return(list(IC_model = IC_model, WS_model = WS_model))
}

models1 <- create_models(sub1data)
models2 <- create_models(sub2data)

# 定义contrast的函数
extract_contrasts <- function(model, vars, groups, contrasts) {
  results <- list()
  for (var in vars) {
    em <- emtrends(model, var = var, specs = ~ Group)
    contrast_results <- lapply(contrasts, function(c) contrast(em, method = list(c)))
    results[[var]] <- lapply(contrast_results, summary)
  }
  return(results)
}

# 定义变量和对比列表
vars <- c("Prior_IC", "Prior_WS", "IC_pcs", "WS_pcs", "Pr_IC", "Pr_WS")
contrasts_3group <- list("PS vs NS" = c(0, -1, 1))
contrasts_4group <- list(
  "PS vs BA" = c(0, -1, 1, 0),
  "PS vs WD" = c(0, -1, 0, 1),
  "BA vs WD" = c(0, 0, -1, 1)
)

# 提取三组对比
results_3group_IC <- extract_contrasts(models1$IC_model, vars, sub1data$Group, contrasts_3group)
results_3group_WS <- extract_contrasts(models1$WS_model, vars, sub1data$Group, contrasts_3group)

# 提取四组对比
results_4group_IC <- extract_contrasts(models2$IC_model, vars, sub2data$Group, contrasts_4group)
results_4group_WS <- extract_contrasts(models2$WS_model, vars, sub2data$Group, contrasts_4group)

# 输出
print_contrast_results <- function(results) {
  for (var in names(results)) {
    cat("\nVariable:", var, "\n")
    for (contrast_name in names(results[[var]])) {
      cat("\nContrast:", contrast_name, "\n")
      print(results[[var]][[contrast_name]])
    }
  }
}

print_contrast_results(results_3group_IC)
print_contrast_results(results_3group_WS)
print_contrast_results(results_4group_IC)
print_contrast_results(results_4group_WS)

#重复相关矩阵
nd_data <- read_csv("C:/Users/ASUS/Desktop/nd_sim_21nov.csv")
correlation_matrices = list()
groups = unique(nd_data$Group)
for (group in groups){
    group_data = subset(nd_data, Group == group)
    numeric_data = group_data[, -1]
    cor_matrix = cor(numeric_data, use= 'pairwise.complete.obs', method = 'pearson')
    correlation_matrices[[group]] <- cor_matrix
}
for (group in groups){
    cat('\nCorrelation matrix for group', group, ':\n')
    lower_tri = correlation_matrices[[group]]
    lower_tri[upper.tri(lower_tri)] <- NA
    print(lower_tri)
}
