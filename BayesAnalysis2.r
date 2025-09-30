library(tidyverse)
library(ggprism)
library(bayestestR)
library(rstanarm)
library(patchwork)  # 用于组合图形

file_path <- "C:/Users/XinHao/Desktop/2025_SCZ_AI/simulation/"
group_levels <- c("HC", "PS", "NS")
fill_colors <- c('#66c2a5','#fc8d62','#8da0cb')

# 读取模拟数据
sim_data <- read_csv(paste0(file_path, 'nd_sim_26aug.csv')) %>%
  filter(Group %in% group_levels) %>%
  mutate(Group = factor(Group, levels = group_levels))

# 准备实证数据
empirical_data <- data.frame(
  Group = factor(rep(c("HC", "PS", "NS"), 
                     times = c(16, 8, 8)), levels = group_levels),
  IC = c(main_data$IC_scaled[1:16], main_data$IC_scaled[17:24], main_data$IC_scaled[25:32]),
  WS = c(main_data$WS_scaled[1:16], main_data$WS_scaled[17:24], main_data$WS_scaled[25:32])
)


# 在代码开始部分，计算每个指标的ROPE
rope_ranges <- list()

for (metric in c("IC", "WS")) {
  # 获取该指标的所有实证数据（不分组）
  all_empirical_values <- empirical_data[[metric]]
  
  # 计算标准差
  metric_sd <- sd(all_empirical_values, na.rm = TRUE)
  
  # 根据Kruschke (2018)计算ROPE：0 ± 0.1 * sd(y)
  rope_margin <- 0.1 * metric_sd
  rope_ranges[[metric]] <- c(-rope_margin, rope_margin)
  
  cat(sprintf("%s: SD = %.4f, ROPE = [%.4f, %.4f]\n", 
              metric, metric_sd, -rope_margin, rope_margin))
}


# 1. 对实证数据进行bootstrapping
n_boot <- 1000
bootstrap_results <- list()

for (metric in c("IC", "WS")) {
  bootstrap_results[[metric]] <- list()
  
  for (group in group_levels) {
    cat("Bootstrapping", metric, "for group", group, "\n")
    emp_original <- empirical_data[empirical_data$Group == group, metric]
    
    # 进行1000次bootstrap，生成bootstrap分布
    bootstrap_samples <- matrix(NA, nrow = n_boot, ncol = length(emp_original))
    for (i in 1:n_boot) {
      bootstrap_samples[i, ] <- sample(emp_original, replace = TRUE)
    }
    
    # 计算每次bootstrap的均值
    bootstrap_means <- apply(bootstrap_samples, 1, mean)
    
    bootstrap_results[[metric]][[group]] <- list(
      bootstrap_means = bootstrap_means,
      original_data = emp_original,
      empirical_mean = mean(emp_original),
      empirical_sd = sd(emp_original)
    )
  }
}

# 2. 获取模拟数据
sim_results <- list()
for (metric in c("IC", "WS")) {
  sim_results[[metric]] <- list()
  for (group in group_levels) {
    sim_results[[metric]][[group]] <- sim_data[sim_data$Group == group, metric][[1]]
  }
}

# 3. 计算组间差异分布
calculate_group_differences <- function(data_list, group_pairs) {
  differences <- list()
  
  for (pair in group_pairs) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    # 计算组间差异
    diff_samples <- data_list[[group1]] - data_list[[group2]]
    
    differences[[paste(group1, "vs", group2)]] <- diff_samples
  }
  
  return(differences)
}

# 定义要比较的组对
group_pairs <- list(
  c("HC", "PS"),
  c("HC", "NS"), 
  c("PS", "NS")
)

# 计算实证bootstrap的组间差异
empirical_differences <- list()
for (metric in c("IC", "WS")) {
  empirical_means_list <- list()
  for (group in group_levels) {
    empirical_means_list[[group]] <- bootstrap_results[[metric]][[group]]$bootstrap_means
  }
  empirical_differences[[metric]] <- calculate_group_differences(empirical_means_list, group_pairs)
}

# 计算模拟数据的组间差异
simulated_differences <- list()
for (metric in c("IC", "WS")) {
  simulated_data_list <- list()
  for (group in group_levels) {
    simulated_data_list[[group]] <- sim_results[[metric]][[group]]
  }
  simulated_differences[[metric]] <- calculate_group_differences(simulated_data_list, group_pairs)
}

# 4. 贝叶斯分析函数（使用统一的ROPE）
perform_bayesian_analysis <- function(data, rope_range) {
  # 使用非信息先验
  model <- stan_glm(value ~ 1, data = data,
                   family = gaussian(),
                   prior = normal(0, 10),
                   prior_intercept = normal(0, 10),
                   prior_aux = exponential(0.1),
                   chains = 4, iter = 2000, seed = 123)
  
  # 获取后验样本
  posterior <- as.data.frame(model)
  intercept_posterior <- posterior$`(Intercept)`
  
  # 计算95% HDI
  hdi_result <- hdi(intercept_posterior, ci = 0.95)
  
  # ROPE分析（使用传入的统一ROPE）
  rope_result <- rope(intercept_posterior, range = rope_range)
  prob_in_rope <- rope_result$ROPE_Percentage / 100
  
  # 判断显著性（基于HDI和ROPE）
  hdi_excludes_zero <- !(hdi_result$CI_low < 0 & hdi_result$CI_high > 0)
  hdi_excludes_rope <- !(hdi_result$CI_low < rope_range[2] & hdi_result$CI_high > rope_range[1])
  
  # 决策规则
  if (prob_in_rope > 0.9) {
    decision <- "Practically equivalent to zero"
    significant <- FALSE
  } else if (hdi_excludes_zero & hdi_excludes_rope) {
    decision <- "Significant and practically important"
    significant <- TRUE
  } else if (hdi_excludes_zero) {
    decision <- "Statistically significant but practically negligible"
    significant <- TRUE
  } else {
    decision <- "Not significant"
    significant <- FALSE
  }
  
  return(list(
    posterior_samples = intercept_posterior,
    posterior_mean = mean(intercept_posterior),
    hdi_lower = hdi_result$CI_low,
    hdi_upper = hdi_result$CI_high,
    rope_lower = rope_range[1],
    rope_upper = rope_range[2],
    prob_in_rope = prob_in_rope,
    decision = decision,
    significant = significant
  ))
}

# 5. 对每个组间差异进行贝叶斯分析（使用统一的ROPE）
group_difference_analysis <- list()

for (metric in c("IC", "WS")) {
  group_difference_analysis[[metric]] <- list()
  
  # 获取该指标的统一ROPE
  metric_rope <- rope_ranges[[metric]]
  
  for (pair_name in names(empirical_differences[[metric]])) {
    cat("Analyzing group differences for", metric, pair_name, "\n")
    
    # 分析实证差异（传入统一的ROPE）
    emp_data <- data.frame(value = empirical_differences[[metric]][[pair_name]])
    emp_results <- perform_bayesian_analysis(emp_data, metric_rope)
    
    # 分析模拟差异（传入统一的ROPE）
    sim_data <- data.frame(value = simulated_differences[[metric]][[pair_name]])
    sim_results_analysis <- perform_bayesian_analysis(sim_data, metric_rope)
    
    # 分析差异的差异（传入统一的ROPE）
    diff_data <- data.frame(
      value = empirical_differences[[metric]][[pair_name]] - simulated_differences[[metric]][[pair_name]]
    )
    diff_diff_results <- perform_bayesian_analysis(diff_data, metric_rope)
    
    group_difference_analysis[[metric]][[pair_name]] <- list(
      empirical = emp_results,
      simulated = sim_results_analysis,
      difference_of_differences = diff_diff_results
    )
  }
}

# 6. 创建左右面板可视化函数（使用统一的ROPE）
create_comparison_panel <- function(metric, pair_name, analysis_results, rope_ranges) {
  results <- analysis_results[[metric]][[pair_name]]
  metric_rope <- rope_ranges[[metric]]
  
  # 准备数据
  emp_data <- data.frame(
    Value = results$empirical$posterior_samples,
    Type = "Empirical Difference"
  )
  
  sim_data <- data.frame(
    Value = results$simulated$posterior_samples,
    Type = "Simulated Difference"
  )
  
  diff_data <- data.frame(
    Value = results$difference_of_differences$posterior_samples,
    Type = "Difference of Differences"
  )
  
  # 左面板：实证差异 vs 模拟差异
  left_plot_data <- rbind(emp_data, sim_data)
  
  left_plot <- ggplot(left_plot_data, aes(x = Value, fill = Type)) +
    geom_density(alpha = 0.6) +
    geom_vline(xintercept = 0, color = "gray50", size = 1, linetype = "dashed") +
    # 添加统一的ROPE区域
    geom_rect(aes(xmin = metric_rope[1], 
                  xmax = metric_rope[2],
                  ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.2, inherit.aes = FALSE) +
    # 添加HDI区间
    geom_segment(data = data.frame(
      Type = c("Empirical Difference", "Simulated Difference"),
      x_start = c(results$empirical$hdi_lower, results$simulated$hdi_lower),
      x_end = c(results$empirical$hdi_upper, results$simulated$hdi_upper),
      y = c(0, 0)
    ), aes(x = x_start, xend = x_end, y = y, yend = y),
    color = "black", size = 3, alpha = 0.8) +
    scale_fill_manual(values = c("Empirical Difference" = "#4daf4a", 
                                "Simulated Difference" = "#377eb8")) +
    theme_prism() +
    labs(x = "Group Difference", y = "Posterior Density",
         title = paste("Group Differences:", pair_name),
         subtitle = paste("Metric:", metric)) +
    theme(legend.position = "bottom")
  
  # 右面板：差异的差异
  right_plot <- ggplot(diff_data, aes(x = Value)) +
    geom_density(fill = "#e41a1c", alpha = 0.6) +
    geom_vline(xintercept = 0, color = "gray50", size = 1, linetype = "dashed") +
    # 添加统一的ROPE区域
    geom_rect(aes(xmin = metric_rope[1],
                  xmax = metric_rope[2],
                  ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.2, inherit.aes = FALSE) +
    # 添加HDI区间
    geom_segment(aes(x = results$difference_of_differences$hdi_lower, 
                     xend = results$difference_of_differences$hdi_upper,
                     y = 0, yend = 0),
                 color = "black", size = 3, alpha = 0.8) +
    theme_prism() +
    labs(x = "Difference of Differences", y = "Posterior Density",
         title = "Difference between Empirical and Simulated Group Differences",
         subtitle = results$difference_of_differences$decision) +
    theme(legend.position = "none")
  
  # 组合图形
  combined_plot <- left_plot + right_plot +
    plot_annotation(
      title = paste("Bayesian Analysis:", metric, "-", pair_name),
      subtitle = paste("Gray area: ROPE [", round(metric_rope[1], 4), ",", 
                      round(metric_rope[2], 4), "] | Black bar: 95% HDI"),
      theme = theme_prism()
    )
  
  return(combined_plot)
}

# 7. 生成并保存所有对比的可视化
for (metric in c("IC", "WS")) {
  for (pair_name in names(group_difference_analysis[[metric]])) {
    cat("Creating visualization for", metric, pair_name, "\n")
    
    panel_plot <- create_comparison_panel(metric, pair_name, group_difference_analysis, rope_ranges)
    
    filename <- paste0(metric, "_", gsub(" ", "_", pair_name), "_comparison.png")
    ggsave(filename = file.path(file_path, filename), 
           plot = panel_plot, width = 16, height = 6, dpi = 500)
  }
}

