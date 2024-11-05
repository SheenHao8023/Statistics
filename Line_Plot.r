library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalt)
library(gsignal)
c

#ITI 折线图
data <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE)
data <- data %>%
    mutate(Participant = rep(c("Role A", "Role B"), each = 8))
data_long <- data %>%
    pivot_longer(cols = ITI1:ITI31, names_to = "ITI", values_to = "Value") %>%
    mutate(ITI = as.numeric(gsub("ITI", "", ITI)))
data_summary <- data_long %>%
    group_by(ITI, Participant) %>%
    summarise(mean = mean(Value, na.rm = TRUE), sd = sd(Value, na.rm = TRUE),
              ymin = mean - sd, ymax = mean + sd, .groups = "drop")
ggplot(data_summary, aes(x = ITI, y = mean, group = Participant, color = Participant)) +
    geom_line(aes(color = Participant), size = 1.5) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Participant), alpha = 0.4, color = "transparent", size = 0) +
    scale_color_manual(values = c("Role A"="#80d6ff","Role B"='#f47c7c')) +
    scale_fill_manual(values = c("Role A"='#80d6ff',"Role B"='#f47c7c')) +
    labs(x = "ITI of dyads", y = "Time (ms)") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "right", 
          axis.line = element_line(linewidth = 0.9, color = "black"), 
          axis.title = element_text(size=14),   
          axis.text = element_text(size=12, color="black"),  
          axis.ticks = element_line(linewidth = 0.9, color = "black")) +
    geom_vline(xintercept = 8, linetype = "dashed", color = "black", size = 0.9) +  # 添加X=8的垂直线
    scale_x_continuous(breaks = c(0, 8, 16, 24, 32))  # 设置横坐标刻度

#RT折线图
dataRT <- data
for (i in 9:31) {
  prev_col_name <- paste0("ITI", i - 1)
  curr_col_name <- paste0("ITI", i)
  dataRT[[curr_col_name]] <- dataRT[[curr_col_name]] + dataRT[[prev_col_name]]}
dataRT <- dataRT %>% 
  select(ITI8:ITI31) %>% 
  rename_at(vars(ITI8:ITI31), ~paste0("RT", 1:24))
last_col_name <- names(data)[ncol(data)] # 获取 data 最后一列的列名
dataRT[[last_col_name]] <- data[[last_col_name]]
dataRT_long <- dataRT %>%
    pivot_longer(cols = RT1:RT24, names_to = "RT", values_to = "Value") %>%
    mutate(RT = as.numeric(gsub("RT", "", RT)))
dataRT_summary <- dataRT_long %>%
    group_by(RT, Participant) %>%
    summarise(mean = mean(Value, na.rm = TRUE), sd = sd(Value, na.rm = TRUE),
              ymin = mean - 3*sd, ymax = mean + 3*sd, .groups = "drop")
ggplot(dataRT_summary, aes(x = RT, y = mean, group = Participant, color = Participant)) +
    geom_line(aes(color = Participant), size = 1.5) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Participant), alpha = 0.4, color = "transparent", size = 0) +
    scale_color_manual(values = c("Role A"="#80d6ff","Role B"='#f47c7c')) +
    scale_fill_manual(values = c("Role A"='#80d6ff',"Role B"='#f47c7c')) +
    labs(x = "RT of dyads", y = "Time (ms)") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "right", 
          axis.line = element_line(linewidth = 0.9, color = "black"), 
          axis.title = element_text(size=14),   
          axis.text = element_text(size=12, color="black"),  
          axis.ticks = element_line(linewidth = 0.9, color = "black")) +
    scale_x_continuous(breaks = seq(0, 24, 6), labels = seq(0, 24, 6))+
    scale_y_continuous(breaks = seq(0, 15000, 2500), labels = seq(0, 15000, 2500))


#三组遮挡的带趋势线的条形图
HC <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 2)
PS <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 3)
NS <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 4)
HC <- HC[, -(1:7)]
names(HC)[names(HC) %in% paste0("ITI", 8:31)] <- paste0("ITI", 1:24)
HC_long <- HC %>%
    pivot_longer(cols = ITI1:ITI24, names_to = "ITI", values_to = "Value") %>%
    mutate(ITI = as.numeric(gsub("ITI", "", ITI)))
PS <- PS[, -(1:7)]
names(PS)[names(PS) %in% paste0("ITI", 8:31)] <- paste0("ITI", 1:24)
PS_long <- PS %>%
    pivot_longer(cols = ITI1:ITI24, names_to = "ITI", values_to = "Value") %>%
    mutate(ITI = as.numeric(gsub("ITI", "", ITI)))
NS <- NS[, -(1:7)]
names(NS)[names(NS) %in% paste0("ITI", 8:31)] <- paste0("ITI", 1:24)
NS_long <- NS %>%
    pivot_longer(cols = ITI1:ITI24, names_to = "ITI", values_to = "Value") %>%
    mutate(ITI = as.numeric(gsub("ITI", "", ITI)))
combined_long <- bind_rows(mutate(HC_long, Group = "HC"),mutate(PS_long, Group = "PS"),mutate(NS_long, Group = "NS")) 
combined_long_mean <- combined_long %>%
  group_by(ITI, Group) %>%
  summarise(Value = mean(Value, na.rm = TRUE)) %>%
  ungroup()
ggplot(combined_long_mean, aes(x = ITI)) +
  geom_bar(data = filter(combined_long_mean, Group == "HC"),aes(y = Value, fill = Group), alpha = 0.8, stat = "identity", position = "identity") +
  geom_bar(data = filter(combined_long_mean, Group == "PS"),aes(y = Value, fill = Group), alpha = 0.8, stat = "identity", position = "identity") +
  geom_bar(data = filter(combined_long_mean, Group == "NS"),aes(y = Value, fill = Group), alpha = 0.8, stat = "identity", position = "identity") +
  geom_smooth(data = filter(combined_long_mean, Group == "HC"),aes(y = Value, color = Group),method = "loess", se = FALSE) +
  geom_smooth(data = filter(combined_long_mean, Group == "PS"),aes(y = Value, color = Group),method = "loess", se = FALSE) +
  geom_smooth(data = filter(combined_long_mean, Group == "NS"),aes(y = Value, color = Group),method = "loess", se = FALSE) +
  scale_fill_manual(values = c("HC" = '#66c2a5', "PS" = '#fc8d62', "NS" = '#8da0cb')) + # 设置条形图颜色
  scale_color_manual(values = c("HC" = '#66c2a5', "PS" = '#fc8d62', "NS" = '#8da0cb'), guide = "none") + # 设置趋势线颜色
  labs(x = "ITI of role B", y = "Time (ms)", fill = "Participant") + # 设置图例标题
  theme_minimal() + # 使用简洁主题
theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "right", 
          axis.line = element_line(linewidth = 0.9, color = "black"), 
          axis.title = element_text(size=14),   
          axis.text = element_text(size=12, color="black"),  
          axis.ticks = element_line(linewidth = 0.9, color = "black")) +
coord_cartesian (ylim = c (350,550)) +
scale_x_continuous(breaks = seq(0, 24, 6), labels = seq(0, 24, 6))

#三组填充颜色的折线图
ggplot(combined_long_mean, aes(x = ITI)) +
  geom_area(data = filter(combined_long_mean, Group == "HC"),aes(y = Value, fill = "HC"), alpha = 0.8) +
  geom_area(data = filter(combined_long_mean, Group == "PS"),aes(y = Value, fill = "PS"), alpha = 0.8) +
  geom_area(data = filter(combined_long_mean, Group == "NS"),aes(y = Value, fill = "NS"), alpha = 0.8) +
  scale_fill_manual(values = c("HC" = '#66c2a5', "PS" = '#fc8d62', "NS" = '#8da0cb')) +
  labs(x = "ITI of role B", y = "Time (ms)", fill = "Participant") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.line = element_line(linewidth = 0.9, color = "black"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"),
        axis.ticks = element_line(linewidth = 0.9, color = "black")) +
  coord_cartesian(ylim = c(350, 550)) +
  scale_x_continuous(breaks = seq(0, 24, 6), labels = seq(0, 24, 6))

#三组误差阴影折线图
combined_long_mean2 <- combined_long %>%
  group_by(ITI, Group) %>%
  summarise(mean = mean(Value, na.rm = TRUE), sd = sd(Value, na.rm = TRUE),ymin = mean - sd, ymax = mean + sd, .groups = "drop") %>%
  ungroup()
ggplot(combined_long_mean2, aes(x = ITI, y = mean, group = Group, color = Group)) +
    geom_line(aes(color = Group), size = 1.5) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Group), alpha = 0.4, color = "transparent", size = 0) +
    scale_color_manual(values = c("HC" = '#66c2a5', "PS" = '#fc8d62', "NS" = '#8da0cb'), guide = "none") +
    scale_fill_manual(values = c("HC" = '#66c2a5', "PS" = '#fc8d62', "NS" = '#8da0cb')) +
    labs(x = "ITI of role B", y = "Time (ms)", fill = "Participant") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "right", 
          axis.line = element_line(linewidth = 0.9, color = "black"), 
          axis.title = element_text(size=14),   
          axis.text = element_text(size=12, color="black"),  
          axis.ticks = element_line(linewidth = 0.9, color = "black")) +
    scale_x_continuous(breaks = c(0, 8, 16, 24))  # 设置横坐标刻度


#圈形图
dataRT$phase_angle <- rep(NA,16)
for (i in 1:16) {
    timeseries = hilbert(sin(2 * pi * 0.5 * as.numeric(dataRT[i, 1:24])))
    dataRT$phase_angle[i] <- atan2(Im(timeseries), Re(timeseries)) * 180 / pi }


