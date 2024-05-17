library(readxl)
mydata <- read_excel("C:/Users/ASUS/Desktop/Fig_Activation.xlsx")
mydata$Group <- as.factor(mydata$Group)
mydata$Condition <- as.character(mydata$Condition)
library(ggplot2)
library(ggprism)
library(RColorBrewer)

# 小提琴与箱线图
ggplot(data = mydata, aes(x = Condition, y = ROI7)) + 
  ylim(-0.06, 0.06) +   # 设定y轴范围
  xlab("Conditions") +  # 设置x轴名称
  ylab("Activation") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑
  geom_boxplot(aes(fill = Group), width=0.3,
               position = position_dodge(0.5),  outlier.color = NA, whisker.width = 0)+
  # 添加统计摘要层以显示平均数
  stat_summary(aes(group = Group), fun.y = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.5)) +
  # stat_summary(aes(group = Group), fun.y = median, geom = "crossbar", width = 0.1, position = position_dodge(0.5)) +
  scale_fill_manual(name = "Legend Title", values = c("#9ADFBF","#D3D3D3"), # 这里需要指定所有使用的颜色
                    labels = c("SZ", "HC")) + # 对应的颜色标签
  scale_x_discrete(labels = c("RS", "HS", "HEO", "HH", "HP"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism() + 
  theme(axis.title.x = element_text(size = 14),  # 改变x轴标题的字体大小
        axis.text.x = element_text(size = 12))   # 改变x轴标签的字体大小

# 小提琴与箱线图
ggplot(data = mydata, aes(x = Condition, y = ROI11)) + 
  ylim(-0.04, 0.08) +   # 设定y轴范围
  xlab("Conditions") +  # 设置x轴名称
  ylab("Activation") +   # 设置y轴名称
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             adjust = 0.75) + # 增加此值可以使密度估计更平滑
  geom_boxplot(aes(fill = Group), width=0.3,
               position = position_dodge(0.5),  outlier.color = NA, whisker.width = 0)+
  # 添加统计摘要层以显示平均数
  stat_summary(aes(group = Group), fun.y = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.5)) +
  # stat_summary(aes(group = Group), fun.y = median, geom = "crossbar", width = 0.1, position = position_dodge(0.5)) +
  scale_fill_manual(name = "Legend Title", values = c("#9ADFBF","#D3D3D3"), # 这里需要指定所有使用的颜色
                    labels = c("SZ", "HC")) + # 对应的颜色标签
  scale_x_discrete(labels = c("RS", "HS", "HEO", "HH", "HP"))+
  guides(fill = guide_legend(title = "Legend Title")) +
  theme_prism() + 
  theme(axis.title.x = element_text(size = 14),  # 改变x轴标题的字体大小
        axis.text.x = element_text(size = 12))   # 改变x轴标签的字体大小

library(dplyr)
# 创建一个新的数据框存储平均值
mean_values <- mydata %>%
  group_by(Condition, Group) %>%
  summarize(mean_activation = mean(ROI11))
# 将数据框与原始数据合并，以便在同一图中绘制
merged_data <- merge(mydata, mean_values, by = c("Condition", "Group"), all.x = TRUE)
merged_data$Group <- as.factor(merged_data$Group)
merged_data$Condition <- as.character(merged_data$Condition)
# 使用合并后的数据绘制小提琴图和散点图
ggplot(data = merged_data, aes(x = Condition, y = ROI11)) + 
  ylim(-0.2, 0.2) +   # 设定y轴范围
  xlab("Conditions") +  # 设置x轴名称
  ylab("Activation") +   # 设置y轴名称
  geom_violin(aes(fill = Group), alpha = 0.9, position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             adjust = 0.75) + # 增加此值可以使密度估计更平滑
  scale_fill_manual(name = "Legend Title", values = c("#9ADFBF","#D3D3D3"), # 这里需要指定所有使用的颜色
                    labels = c("SZ", "HC")) + # 对应的颜色标签
  scale_x_discrete(labels = c("RS", "HS", "HEO", "HH", "HP"))+  # 更改x轴标签
  guides(fill = guide_legend(title = "Legend Title"), color = guide_legend(title = "Data Type")) +
  theme_prism() + 
  theme(axis.title.x = element_text(size = 14),  # 改变x轴标题的字体大小
        axis.text.x = element_text(size = 12))   # 改变x轴标签的字体大小






library(readxl)
library(ggplot2)
library(ggprism)
library(RColorBrewer)
library(ggstance)

mydata <- read_excel("C:/Users/ASUS/Desktop/Fig_Activation.xlsx")
mydata$Group <- as.factor(mydata$Group)
mydata$Condition <- as.character(mydata$Condition)
Activationcolors <- c("#9ADFBF", "#D3D3D3")  # 用你想要的十六进制颜色
Grangercolors <- c("#9BD3EF", "#D3D3D3")
Degreecolors <- c("#FFD699", "#D3D3D3")

ggplot(mydata, aes(x = Condition, y = ROI7)) +
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA,      # 设置边框为透明
             alpha = 0.5,    # 可选：调整填充的透明度，1为完全不透明，0为完全透明
             # draw_quantiles = c(0.01, 0.99), # 只绘制两端的分位数线，使得边缘更平滑
             trim = FALSE, adjust = 0.75) + # 增加此值可以使密度估计更平滑
  # introdataviz::geom_split_violin(aes(fill = Group), alpha =0.5, position = position_dodge(0.7), 
        # width=1.2, trim = FALSE, color = NA, adjust = 0.75) + 
  geom_boxplot(aes(fill = Group), width =0.3, alpha =1, outlier.color = NA, 
        fatten = NULL, show.legend = FALSE) +  # 添加箱线图
  stat_summary(aes(group = Group), fun.y = mean, geom = "crossbar", width=0.28, show.legend = F, 
               position = position_dodge(0.3)) +  # 绘制均值和标准误
  scale_x_discrete(name = "Conditions", labels = c("RS", "HS", "HEO", "HH", "HP")) +  # X轴条件标签
  scale_y_continuous(name = "Activation",  # Y轴激活水平
                     breaks = seq(-0.2, 0.2, 0.1), 
                     limits = c(-0.2, 0.2)) +
  scale_fill_manual(values = Activationcolors, name = "Group", labels = c("SZ", "HC")) +  # 使用自定义颜色
  theme_minimal() +  # 使用最小主题
  theme(axis.title.x = element_text(size = 14),  # 改变轴标题的字体大小
        axis.text.x = element_text(size = 12),   # 改变轴标签的字体大小
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12))   

ggplot(mydata, aes(x = Condition, y = ROI11)) +
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA, alpha = 0.5, trim = FALSE, adjust = 0.75) + 
  geom_boxplot(aes(fill = Group), width =0.3, alpha =1, outlier.color = NA, 
        fatten = NULL, show.legend = FALSE) +
  stat_summary(aes(group = Group), fun.y = mean, geom = "crossbar", width=0.28, show.legend = F, 
               position = position_dodge(0.3)) + 
  scale_x_discrete(name = "Conditions", labels = c("RS", "HS", "HEO", "HH", "HP")) +  
  scale_y_continuous(name = "Activation", 
                     breaks = seq(-0.2, 0.2, 0.1), 
                     limits = c(-0.2, 0.2)) +
  scale_fill_manual(values = Activationcolors, name = "Group", labels = c("SZ", "HC")) + 
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12))  

ggplot(mydata, aes(x = Condition, y = Degree)) +
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA, alpha = 0.5, trim = FALSE, adjust = 0.75) + 
  geom_boxplot(aes(fill = Group), width =0.3, alpha =1, outlier.color = NA, 
        fatten = NULL, show.legend = FALSE) +
  stat_summary(aes(group = Group), fun.y = mean, geom = "crossbar", width=0.28, show.legend = F, 
               position = position_dodge(0.3)) + 
  scale_x_discrete(name = "Conditions", labels = c("RS", "HS", "HEO", "HH", "HP")) +  
  scale_y_continuous(name = "Nodal Degree Centrality", 
                     breaks = seq(-1, 4, 1), 
                     limits = c(-1, 4)) +
  scale_fill_manual(values = Degreecolors, name = "Group", labels = c("SZ", "HC")) + 
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12))

ggplot(mydata, aes(x = Condition, y = Rank)) +
  geom_violin(aes(fill = Group), position = position_dodge(0.5), width=1.5,
             color = NA, alpha = 0.5, trim = FALSE, adjust = 0.75) + 
  geom_boxplot(aes(fill = Group), width =0.3, alpha =1, outlier.color = NA, 
        fatten = NULL, show.legend = FALSE) +
  stat_summary(aes(group = Group), fun.y = mean, geom = "crossbar", width=0.28, show.legend = F, 
               position = position_dodge(0.3)) + 
  scale_x_discrete(name = "Conditions", labels = c("RS", "HS", "HEO", "HH", "HP")) +  
  scale_y_continuous(name = "Left Frontopolar Rank", 
                     breaks = seq(-2, 10, 3), 
                     limits = c(-2,10)) +
  scale_fill_manual(values = Degreecolors, name = "Group", labels = c("SZ", "HC")) + 
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12))  

rankdata <- read_excel("C:/Users/ASUS/Desktop/Rank.xlsx")
rankdata$Group <- as.character(rankdata$Group) 
ggplot(rankdata, aes(x = Group, y = Rank)) +
  geom_violin(width=1.5, position = position_dodge(0.5), 
             color = NA, alpha = 0.5, trim = FALSE, adjust = 0.75) + 
#  geom_boxplot( width =0.3, alpha =1, outlier.color = NA, 
#        fatten = NULL, show.legend = FALSE) +
#  stat_summary(fun.y = mean, geom = "crossbar", width=0.28, show.legend = F, 
#               position = position_dodge(0.3)) + 
  scale_x_discrete(name = "Groups", labels = c("SZ", "HC")) +  
  scale_y_continuous(name = "Nodal Degree Rank", 
                     breaks = seq(-10, 20, 2), 
                     limits = c(-10,20)) +
  scale_fill_manual(values = Degreecolors, name = "Group", labels = c("SZ", "HC")) + 
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),  
        axis.text.y = element_text(size = 12))