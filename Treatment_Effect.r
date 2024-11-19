library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggprism)

data <- read_excel("C:/Users/ASUS/Desktop/FollowUp.xlsx", col_names = TRUE, sheet = 1)


data_condition <- data %>% select(Group, ends_with("C1"))  # 保留Group列和以C1结尾的列
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Condition), y = value, fill = factor(Group))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = "black", size = 0.5) + 
  geom_jitter(aes(color = factor(Group)), size = 2, width = 0.2, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = 'Hearing each other', y = "Consistency", fill = "Group") +
  scale_fill_manual(values = c('#80d6ff', '#f47c7c')) +  # Group 1 和 Group 2的颜色

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
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c'))

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
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c'))

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
  scale_fill_manual(values = c('#f47c7c', '#f47c7c', '#f47c7c'))



data <- read_excel("C:/Users/ASUS/Desktop/FollowUp.xlsx", col_names = TRUE, sheet = 2)


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
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff'))

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
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff'))

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
  scale_fill_manual(values = c('#80d6ff', '#80d6ff', '#80d6ff'))

