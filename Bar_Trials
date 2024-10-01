library(ggplot2)

values <- c(0.78,0.82,0.82,0.85,0.88,0.16,0.27,0.86,0.51,0.75)  
df <- data.frame(Trial = factor(c(1:10)),  Value = values)

p <- ggplot(df, aes(x=Trial, y=Value, fill=Trial == "7")) +  # 使用条件语句设定填充颜色
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +  # 条形图
  scale_fill_manual(values=c("#66C2A5", "#FC8D62"),  guide=FALSE) +  # 不显示图例
  theme_minimal() +  # 应用简约主题
  theme(
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    axis.line = element_line(color="gray", size=0.5),  # 轴线颜色和粗细
    panel.grid.major = element_line(color="lightgray", size=0.2),  # 主网格线
    panel.grid.minor = element_blank(),  # 次网格线
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  labs(title="Bar Chart with Custom Colors", x="Trial", y="Consistency") +  # 设置图表标题和轴标签
  ylim(0, 1)  # 设置纵坐标范围
print(p)
