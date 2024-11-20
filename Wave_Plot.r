# 加载必要的包
library(ggplot2)

# 生成数据
x <- seq(0, 10, length.out = 500) # X 轴范围为 0 到 10，分500点

# 定义两个函数，振幅逐渐减小，周期调整
amplitude1 <- 1 - 0.07 * x # 振幅1，线性递减
amplitude2 <- 1.5 - 0.11 * x # 振幅2，线性递减

# 定义两个函数的波形，先 in phase，后 out of phase
transition_point <- length(x) * 0.5 # 转换点，前50% in phase，后50% out of phase
phase_shift <- ifelse(x <= max(x) / 2, 0, pi * (x - max(x) / 2) / (max(x) / 2)) # 动态相位差

y1 <- amplitude1 * sin(2 * pi * x / 6) # 第一个函数
y2 <- amplitude2 * sin(2 * pi * x / 6 + phase_shift) # 第二个函数

# 线性增长的偏移量
offset <- (x - max(x)/2) / (max(x)/2) * 0.5 # 平滑的偏移量，逐渐增加

# 只调整红色线后半部分的末端，使其平滑向上漂移
y2_adjusted <- y2 + offset # 在后半段加上平滑偏移量

# 整体向上平移两个单位
y1 <- y1 + 1.5
y2_adjusted <- y2_adjusted + 1.5

# 组织为数据框
data <- data.frame(
    x = rep(x, 2),
    y = c(y1, y2_adjusted),
    Participant = rep(c("Role A", "Role B"), each = length(x))
)

# 绘图
ggplot(data, aes(x = x, y = y, color = Participant, group = Participant)) +
    geom_line(size = 1.5) + # 设置线的宽度
    scale_color_manual(values = c("Role A" = "#80d6ff", "Role B" = "#f47c7c")) + # 设置颜色
    labs(x = "Time", y = "Phase Angle", color = "Participant") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(), # 去除刻度文字
        axis.ticks = element_blank(), # 去除刻度线
        axis.line = element_line(linewidth = 0.9, color = "black"), # 坐标轴
        axis.title = element_text(size = 14), # 坐标轴标题大小
        legend.position = "right", # 图例位置
        plot.title = element_blank() # 去除图片标题
    ) +
    coord_cartesian(ylim = c(-0.5, 3), xlim = c(0, 9)) # 限制y轴范围，并尽量保留右下角空白
