library(ggplot2)
library(readr)
library(ggExtra)
library(bruceR)
library(emmeans)

# Load data
file_path <- "C:\\Users\\haox8\\OneDrive\\桌面\\AI_SCZ\\paper_Lexi\\simulation\\"
nd_data <- read_csv(paste0(file_path, 'nd_sim_23jan.csv'))
sub1data <- subset(nd_data, Group %in% c('HC', 'PS', 'NS'))
sub1data$Group <- factor(sub1data$Group, levels = c('HC', 'PS', 'NS'))
sub2data <- subset(nd_data, Group %in% c('HC', 'PS', 'BA', 'WD'))
sub2data$Group <- factor(sub2data$Group, levels = c('HC', 'PS', 'BA', 'WD'))
colors_3group <- c('HC' = '#66c2a5', 'PS' = '#fc8d62', 'NS' = '#8da0cb')
colors_4group <- c('HC' = '#66c2a5', 'PS' = '#fc8d62', 'BA' = '#e78ac3', 'WD' = '#a6d854')

# 1. print correlation matrix (no FDR correction)
correlation_matrices <- list()
pvalue_matrices <- list()
groups <- unique(nd_data$Group)
for (group in groups) {
    group_data <- subset(nd_data, Group == group)
    numeric_data <- group_data[, -1]  # 去掉分组列，仅保留数值型数据
    # 计算相关矩阵和P值矩阵
    cor_matrix <- matrix(NA, ncol=ncol(numeric_data), nrow=ncol(numeric_data))
    p_matrix <- matrix(NA, ncol=ncol(numeric_data), nrow=ncol(numeric_data))
    colnames(cor_matrix) <- colnames(numeric_data)
    rownames(cor_matrix) <- colnames(numeric_data)
    colnames(p_matrix) <- colnames(numeric_data)
    rownames(p_matrix) <- colnames(numeric_data)
    for (i in 1:ncol(numeric_data)) {
        for (j in i:ncol(numeric_data)) {  # 仅计算下三角部分
            test_result <- cor.test(numeric_data[[i]], numeric_data[[j]], use='pairwise.complete.obs', method='pearson')
            cor_matrix[i, j] <- test_result$estimate
            cor_matrix[j, i] <- test_result$estimate  # 对称
            
            p_matrix[i, j] <- test_result$p.value
            p_matrix[j, i] <- test_result$p.value  # 对称
        }
    }
    p_matrix_adj <- p.adjust(p_matrix[lower.tri(p_matrix)], method="bonferroni")
    p_matrix[lower.tri(p_matrix)] <- p_matrix_adj
    p_matrix[upper.tri(p_matrix)] <- NA  # 仅保留下三角
    correlation_matrices[[group]] <- cor_matrix
    pvalue_matrices[[group]] <- p_matrix
}

for (group in groups) {
    cat("\nCorrelation matrix for group", group, ":\n")
    lower_tri <- correlation_matrices[[group]]
    lower_tri[upper.tri(lower_tri)] <- NA
    print(lower_tri)
}
for (group in groups) {
    cat("\nBonferroni corrected P-value matrix for group", group, ":\n")
    p_matrix_formatted <- apply(pvalue_matrices[[group]], c(1,2), function(x) 
        ifelse(is.na(x), NA, formatC(x, format='f', digits=4)))
    print(p_matrix_formatted)
}



# 2. print GLM model
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

#3. print contrasts
extract_contrasts <- function(model, vars, groups, contrasts) {
  results <- list()
  for (var in vars) {
    em <- emtrends(model, var = var, specs = ~ Group)
    contrast_results <- lapply(contrasts, function(c) contrast(em, method = list(c)))
    summary_results <- lapply(contrast_results, summary)
    p_values <- sapply(summary_results, function(res) res$p.value)
    p_values_fdr <- p.adjust(p_values, method = "bonferroni")
    for (i in seq_along(summary_results)) {
      summary_results[[i]]$p.value <- p_values_fdr[i]
    }
    results[[var]] <- summary_results
  }
  return(results)
}

# em_prior_ic <- emtrends(IC_model1, var = "Prior_IC", specs = ~ Group)
# contrast_prior_ic <- contrast(em_prior_ic, method = list("PS vs NS" = c(0, -1, 1)))
# em_prior_ws <- emtrends(IC_model1, var = "Prior_WS", specs = ~ Group)
# contrast_prior_ws <- contrast(em_prior_ws, method = list("PS vs NS" = c(0, -1, 1)))
# summary(contrast_prior_ic)
# summary(contrast_prior_ws)

vars <- c("Prior_IC", "Prior_WS", "IC_pcs", "WS_pcs", "Pr_IC", "Pr_WS")
outcomes <- c("IC", "WS")
contrasts_3group <- list("HC vs PS" = c(-1, 1, 0),"HC vs NS" = c(-1, 0, 1),"PS vs NS" = c(0, -1, 1))
contrasts_4group <- list(
  "HC vs PS" = c(-1, 1, 0, 0),  #HC为参考的contrast系数和回归模型中系数一致
  "HC vs BA" = c(-1, 0, 1, 0),
  "HC vs WD" = c(-1, 0, 0, 1),
  "PS vs BA" = c(0, -1, 1, 0),
  "PS vs WD" = c(0, -1, 0, 1),
  "BA vs WD" = c(0, 0, -1, 1)
)

results_3group_IC <- extract_contrasts(models1$IC_model, vars, sub1data$Group, contrasts_3group)
results_3group_WS <- extract_contrasts(models1$WS_model, vars, sub1data$Group, contrasts_3group)
results_4group_IC <- extract_contrasts(models2$IC_model, vars, sub2data$Group, contrasts_4group)
results_4group_WS <- extract_contrasts(models2$WS_model, vars, sub2data$Group, contrasts_4group)

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

# 定义函数，根据对比结果提取 p 值并生成矩阵
create_significance_matrix <- function(results, groups, contrasts, vars) {
  significance_matrix <- matrix(0, nrow = length(groups), ncol = length(vars))
  rownames(significance_matrix) <- groups
  colnames(significance_matrix) <- vars
  for (var in vars) {
    for (contrast_name in names(contrasts)) {
      p_val <- results[[var]][[contrast_name]]$p.value  
      if (p_val < 0.05) {
        contrast_vec <- contrasts[[contrast_name]]  
        selected_groups <- which(contrast_vec != 0)  
        significance_matrix[selected_groups, var] <- 1 
      }
    }
  }
  return(significance_matrix)
}

matrix_3group_IC <- create_significance_matrix(results_3group_IC, c("HC", "PS", "NS"), contrasts_3group, vars)
matrix_3group_WS <- create_significance_matrix(results_3group_WS, c("HC", "PS", "NS"), contrasts_3group, vars)
matrix_4group_IC <- create_significance_matrix(results_4group_IC, c("HC", "PS", "BA", "WD"), contrasts_4group, vars)
matrix_4group_WS <- create_significance_matrix(results_4group_WS, c("HC", "PS", "BA", "WD"), contrasts_4group, vars)

# 4. save scatter plots
generate_scatter_plots <- function(data, group_colors, group_name, outcome_matrix, save_path) {
  for (predictor in vars) {
    p <- ggplot(data, aes_string(x = predictor, y = sub(".*_", "", group_name), color = "Group")) +
      geom_point(size = 2, alpha = 0.5, shape = 1) + 
      geom_smooth(data = subset(data, Group %in% rownames(outcome_matrix)[outcome_matrix[, predictor] == 1]),
                  method = "lm", se = FALSE, aes_string(color = "Group")) + 
      scale_color_manual(values = group_colors) +
      theme_minimal(base_family = "sans") +
      coord_cartesian(
          ylim = c(0, 1),
          xlim = if (predictor %in% c("Pr_IC", "Pr_WS")) c(0, 1.50) else c(0, 1)
      ) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none"  
      ) +
      labs(x = predictor, y = sub(".*_", "", group_name))
# 5. print slopes
  for (group in unique(data$Group)) {
      subset_data <- subset(data, Group == group)
      model <- lm(as.formula(paste(sub(".*_", "", group_name), "~", predictor)), data = subset_data)
      slope <- coef(model)[2]  # 提取斜率
      cat(sprintf("Group: %s, Predictor: %s, Slope: %.4f\n", group, predictor, slope))
  }
  p <- ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)
    file_name <- paste0(save_path, "/scatter_", group_name, "_", predictor, ".png")
    ggsave(file_name, plot = p, width = 5, height = 5, dpi = 300, create.dir = TRUE)
  }
}
generate_scatter_plots(sub1data, colors_3group, "3group_IC", matrix_3group_IC, file_path)
generate_scatter_plots(sub1data, colors_3group, "3group_WS", matrix_3group_WS, file_path)
generate_scatter_plots(sub2data, colors_4group, "4group_IC", matrix_4group_IC, file_path)
generate_scatter_plots(sub2data, colors_4group, "4group_WS", matrix_4group_WS, file_path)
