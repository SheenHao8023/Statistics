library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(complex)

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


#三组带趋势线的条形图
HC <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 2)
PS <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 3)
NS <- read_excel("C:/Users/ASUS/Desktop/ITI.xlsx", col_names = TRUE, sheet = 4)
HC <- HC[, -(1:8)]
HC_long <- HC %>%
    pivot_longer(cols = ITI9:ITI31, names_to = "ITI", values_to = "Value") %>%
    mutate(ITI = as.numeric(gsub("ITI", "", ITI)))
set.seed(123) # 为了可重复性
df <- data.frame(
  group = rep(c("Group1", "Group2", "Group3"), each = 10),
  x = rep(1:10, 3),
  value = c(rnorm(10, 5, 2), rnorm(10, 7, 2), rnorm(10, 6, 2))
)

# 创建条形图，每组数据的条形图叠加在一起
p <- ggplot(df, aes(x = x)) +
  geom_bar(aes(y = value, fill = group), stat = "identity", position = "identity", width = 0.25) +
  geom_smooth(aes(y = value, color = group, group = group), method = "loess", se = FALSE, aes(shape = group)) +
  scale_fill_manual(values = c("Group1" = "red", "Group2" = "green", "Group3" = "blue")) + # 设置条形图颜色
  scale_color_manual(values = c("Group1" = "red", "Group2" = "green", "Group3" = "blue")) + # 设置趋势线颜色
  scale_shape_manual(values = c(1, 2, 3)) + # 设置趋势线形状
  labs(color = "Group", shape = "Group") + # 设置图例标题
  theme_minimal() + # 使用简洁主题
  theme(legend.position = "bottom") # 设置图例位置

# 显示图表
print(p)





dataRT <- data %>%
  mutate(across(ITI1:ITI31, ~ifelse(is.na(.), 500, .)))
for (i in 2:31) {
  prev_col_name <- paste0("ITI", i - 1)
  curr_col_name <- paste0("ITI", i)
  dataRT[[curr_col_name]] <- dataRT[[curr_col_name]] + dataRT[[prev_col_name]]}
phase_data <- dataRT %>%
  rowwise() %>%
  mutate(Hilbert_Transform = list(hilbert(c_across(ITI1:ITI31)))) %>%
  mutate(Phase_Angle = atan2(Im(Hilbert_Transform[[1]]), Re(Hilbert_Transform[[1]])) * (180 / pi)) %>%
  ungroup()
phase_data <- dataRT %>% 
  rowwise() %>% 
  mutate(across(ITI1:ITI31, ~ (.-mean(c_across(ITI1:ITI31)))/sd(c_across(ITI1:ITI31)))) %>% 
  mutate(Phase = list(exp(1i * c_across(ITI1:ITI31) * 2 * pi / max(c_across(ITI1:ITI31))))) %>%
  unnest_wider(Phase, names_sep = "_") %>%
  select(-starts_with("ITI"))
# 计算相位角
phase_angles <- phase_data %>%
  rowwise() %>%
  mutate(Phase_Angle = atan2(Re(Phase_1), Im(Phase_1)) * (180 / pi)) %>%
  select(Phase_Angle)



