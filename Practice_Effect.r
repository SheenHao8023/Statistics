library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggprism)

data <- read_excel("C:/Users/haox8/Desktop/Consistency.xlsx", col_names = TRUE)
MANOVA(data, dvs = "B1C1:B5C3", dvs.pattern = "B(.)C(.)", 
        within = c("Block", "Condition"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3)

data_condition <- data %>% select(ends_with("C1"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x='Hearing each other', y="Consistency") +
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c'))

data_condition <- data %>% select(ends_with("C2"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x='Hearing A', y="Consistency") +
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c'))

data_condition <- data %>% select(ends_with("C3"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x='Hearing B', y="Consistency") +
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c', '#f47c7c'))

#Time series
data_time <- read_excel("C:/Users/haox8/Desktop/Consistency.xlsx", col_names = TRUE, sheet = 2)
data_summary <- data_time %>%
  select(T01:T15) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c('Time', '.value'), names_sep = '_')
data_summary <- data_summary %>%
  mutate(ymin = mean-sd, ymax = mean+sd)
data_summary$Time = factor(data_summary$Time, levels = unique(data_summary$Time))
ggplot(data_summary, aes(x = Time, y = mean, group = 1)) +
  geom_line(color = "#f47c7c", size = 1.5) +
  geom_ribbon(data = data_summary %>% filter(!is.na(sd)), aes(ymin = ymin, ymax = ymax), fill = "#f47c7c", alpha = 0.2) +
  labs(x = "Time", y = "Consistency") +
  theme_minimal() +
  theme_prism(axis_text_angle = 45) + 
  coord_cartesian(ylim = c(0, 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "right", 
        axis.line = element_line(linewidth = 0.9, color = "black"), 
        axis.title = element_text(size=14),   
        axis.text = element_text(size=12, color="black"),  
        axis.ticks = element_line(linewidth = 0.9, color = "black"))

data <- read_excel("C:/Users/haox8/Desktop/Stability.xlsx", col_names = TRUE)
MANOVA(data, dvs = "B1C1:B5C3", dvs.pattern = "B(.)C(.)", 
        within = c("Block", "Condition"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3)

data_condition <- data %>% select(ends_with("C1"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x='Hearing each other', y="Stability") +
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff'))

data_condition <- data %>% select(ends_with("C2"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x='Hearing A', y="Stability") +
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff'))

data_condition <- data %>% select(ends_with("C3"))
data_long <- data_condition %>%
  pivot_longer(cols = everything(), names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = Condition))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.5)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.8)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.position = 'right',
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x='Hearing B', y="Stability") +
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff', '#80d6ff'))

#Time series
data_time <- read_excel("C:/Users/haox8/Desktop/Stability.xlsx", col_names = TRUE, sheet = 2)
data_summary <- data_time %>%
  select(T01:T15) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c('Time', '.value'), names_sep = '_')
data_summary <- data_summary %>%
  mutate(ymin = mean-sd, ymax = mean+sd)
data_summary$Time = factor(data_summary$Time, levels = unique(data_summary$Time))
ggplot(data_summary, aes(x = Time, y = mean, group = 1)) +
  geom_line(color = "#80d6ff", size = 1.5) +
  geom_ribbon(data = data_summary %>% filter(!is.na(sd)), aes(ymin = ymin, ymax = ymax), fill = "#80d6ff", alpha = 0.2) +
  labs(x = "Time", y = "Stability") +
  theme_minimal() +
  theme_prism(axis_text_angle = 45) + 
  coord_cartesian(ylim = c(0, 0.2)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "right", 
        axis.line = element_line(linewidth = 0.9, color = "black"), 
        axis.title = element_text(size=14),   
        axis.text = element_text(size=12, color="black"),  
        axis.ticks = element_line(linewidth = 0.9, color = "black"))
