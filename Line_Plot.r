library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(complex)

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

library(ggplot2)
library(dplyr)

# 假设 phase_angles 是一个包含16个值的向量
# 示例数据
phase_angles <- data.frame(angle = c(runif(8, 0, 360), runif(8, 0, 360))) # 这里用随机值替代，实际使用时请替换为你的数据

# 将角度分为两组
angles <- data.frame(
  angle = c(phase_angles$angle[1:8], phase_angles$angle[9:16]),
  group = rep(c("Participant A", "Participant B"), each = 8)
)

# 创建同心圆的数据
angles <- angles %>%
  mutate(radius = rep(1:8, times = 2)) %>%
  mutate(x = radius * cos(angle * (pi / 180)),
         y = radius * sin(angle * (pi / 180)))

# 绘制同心圆图
ggplot(angles, aes(x = x, y = y, group = interaction(group, radius), color = group)) +
  geom_polygon(fill = NA, size = 1.2, alpha = 0.5) +
  coord_fixed() +  # 保持圆形比例
  scale_color_manual(values = c("Participant A" = "#80d6ff", "Participant B" = "#f47c7c")) +
  labs(title = "Phase Angles of Participants A and B", x = "X-axis", y = "Y-axis", color = "Participant") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


