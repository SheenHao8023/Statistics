library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggprism)
library(bruceR)

data <- read_excel("C:/Users/ASUS/Desktop/FollowUp.xlsx", col_names = TRUE, sheet = 1)
MANOVA(data, dvs = "S1C1:S3C3", dvs.pattern = "S(.)C(.)", 
        between = 'Group', within = c("Session", "Condition"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = 'C:/Users/ASUS/Desktop/Results1.doc')

data_condition <- data %>% select(Group, ends_with("C1"))  # 保留Group列和以C1结尾的列
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = 'Hearing each other', y = "Consistency", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))

data_condition <- data %>% select(Group, ends_with("C2")) 
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = 'Hearing A', y = "Consistency", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))

data_condition <- data %>% select(Group, ends_with("C3")) 
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = 'Hearing B', y = "Consistency", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))

               
data <- read_excel("C:/Users/ASUS/Desktop/FollowUp.xlsx", col_names = TRUE, sheet = 2)
MANOVA(data, dvs = "S1C1:S3C3", dvs.pattern = "S(.)C(.)", 
        between = 'Group', within = c("Session", "Condition"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = 'C:/Users/ASUS/Desktop/Results2.doc')

              
data_condition <- data %>% select(Group, ends_with("C1")) 
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x = 'Hearing each other', y = "Stability", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))

data_condition <- data %>% select(Group, ends_with("C2")) 
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x = 'Hearing A', y = "Stability", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))


data_condition <- data %>% select(Group, ends_with("C3")) 
data_long <- data_condition %>% 
  pivot_longer(cols = -Group, names_to = "Condition", values_to = "value")
ggplot(data_long, aes(x = factor(Group, labels = c('Fix', 'Random')), y = value, fill = factor(Condition))) + 
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.7), width = 0.6, colour = NA, size = 0.5) + 
  geom_jitter(color = 'black', size = 1, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", position = position_dodge(width = 0.7), width = 0.3, size = 0.8) + 
  theme_prism(axis_text_angle = 0) + 
  theme(legend.position = 'right',
        legend.direction = "vertical",
        axis.text = element_text(size=12, color="black"), 
        axis.title = element_text(size=14, face="bold"),   
        plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(x = 'Hearing B', y = "Stability", fill = "Session") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), labels = c('Session1', 'Session2', 'Session3'))
