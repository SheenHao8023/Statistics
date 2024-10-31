library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

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