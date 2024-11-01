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
  summarise(mean = mean(Value, na.rm = TRUE),sd = sd(Value, na.rm = TRUE),
            ymin = mean - sd,ymax = mean + sd,.groups = "drop")
ggplot(data_summary, aes(x = ITI, y = mean, group = Participant, color = Participant)) +
  geom_line(aes(color = Participant), size = 1.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Participant),alpha = 0.4,color = "transparent",size = 0) +
  scale_color_manual(values = c("Role A"="#80d6ff","Role B"='#f47c7c')) +
  scale_fill_manual(values = c("Role A"='#80d6ff',"Role B"='#f47c7c')) +
  labs(x = "ITI", y = "Time (ms)", color = "Participant", fill = "Participant") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), # 移除主要网格线
        panel.grid.minor = element_blank(), # 移除次要网格线
        legend.position = "right", # 图例放在右侧
        axis.line = element_line(linewidth = 0.9, color = "black"), # 黑色坐标轴线
        axis.title = element_text(size=14),   # 轴标题大小和样式
        axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
        axis.ticks = element_line(linewidth = 0.9, color = "black")) 

#Stability柱状图
iti_data <- data[1, 8:31]
iti_data <- data.frame(Index = 1:24, Value = as.numeric(iti_data))
ggplot(iti_data, aes(x = Index, y = Value, fill = factor(1))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = "#f47c7c", name = "Role B") +
    scale_x_continuous(breaks = c(0,8,16,24)) + # 设置横坐标最多标注到24
    coord_cartesian(ylim = c(350,550))+ 
    theme_minimal() +
    labs(x = "ITI", y = "Time (ms)") +
    theme(
      panel.grid.major = element_blank(), # 移除主要网格线
      panel.grid.minor = element_blank(), # 移除次要网格线
      axis.line = element_line(linewidth = 0.9, color = "black"), # 黑色坐标轴线
      axis.title = element_text(size = 14), # 轴标题大小和样式
      axis.text = element_text(size = 12, color = "black"), # 轴文本大小和颜色
      axis.ticks = element_line(linewidth = 0.9, color = "black"), # 坐标轴刻度线样式
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))


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



