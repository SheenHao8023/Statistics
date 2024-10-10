library(ggplot2)
library(reshape2)
library(dplyr)

#去除异常值，作图SZconsistency
data <- data.frame(
  S07221 = c(0.56, 0.44, 0.70, 0.61, 0.31, 0.15, 0.27, 0.31),
  S07291 = c(0.31, 0.14, 0.19, 0.41, 0.25, 0.14, 0.15, 0.33),
  S08032 = c(0.82, 0.23, 0.18, 0.34, 0.06, 0.62, 0.46, 0.58),
  S08173 = c(0.73, 0.42, 0.45, 0.38, 0.78, 0.72, 0.61, 0.69),
  S08261 = c(0.37, 0.40, 0.49, 0.60, 0.68, 0.55, 0.69, 0.42),
  S09151 = c(0.51, 0.36, 0.76, 0.22, 0.30, 0.45, 0.22, 0.47),
  S09173 = c(0.23, 0.05, 0.14, 0.11, 0.10, 0.26, 0.19, 0.18),
  S09191 = c(0.55, 0.20, 0.69, 0.62, 0.74, 0.50, 0.80, 0.67),
  S09271 = c(0.43, 0.46, 0.46, 0.64, 0.32, 0.36, 0.23, 0.88),
  S10221 = c(0.70, 0.51, 0.76, 0.63, 0.16, 0.30, 0.44, 0.29),
  S11151 = c(0.52, 0.49, 0.47, 0.34, 0.21, 0.56, 0.24, 0.32),
  S11161 = c(0.59, 0.80, 0.21, 0.49, 0.82, 0.60, 0.86, 0.07),
  S11162 = c(0.44, 0.51, 0.61, 0.30, 0.33, 0.34, 0.29, 0.40),
  S08031 = c(0.34, 0.24, 0.36, 0.37, 0.32, 0.35, 0.17, 0.26),
  S09192 = c(0.30, 0.15, 0.40, 0.21, 0.45, 0.42, 0.65, 0.73),
  S08171 = c(0.21, 0.16, 0.11, 0.65, 0.03, 0.50, 0.28, 0.07),
  S09171 = c(0.33, 0.63, 0.37, 0.05, 0.65, 0.20, 0.13, 0.37))
statistic_raw <- matrix(nrow = ncol(data), ncol = 2) #原始数据的均值方差
for (j in 1:ncol(data)) {
  statistic_raw[j, ] <- c(mean(data[, j], na.rm = TRUE), var(data[, j], na.rm = TRUE))}
statistic <- matrix(nrow = ncol(data), ncol = 2)
colnames(statistic) <- c("Mean", "Variance")
for (i in 1:ncol(data)) {
  current_data <- data[, i]
  threshold <- 3 * mad(current_data, constant = 1)  # 使用constant = 1来得到原始MAD,定义异常值的阈值，N倍的MAD
  abs_devs <- abs(current_data - median(current_data, na.rm = TRUE))
  non_outliers_data <- current_data[which(abs_devs <= threshold)]
  data[setdiff(1:nrow(data), which(abs_devs <= threshold)), i] <- NA
  statistic[i, ] <- c(mean(non_outliers_data, na.rm = TRUE), var(non_outliers_data, na.rm = TRUE))}

data_long <- melt(data)
data_long <- data_long %>%
  mutate(group = ifelse(variable %in% colnames(data)[1:13], "Group1", "Group2"))
p <- ggplot(data_long, aes(x = variable, y = value, fill = group)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, lwd = 0.8, fatten = 1.5) + # 调整箱子宽度和误差线粗细
  scale_fill_manual(values = c("Group1" = "#4DBBD4", "Group2" = "#E64A35"), guide = FALSE) + # 蓝色#4DBBD4  HC绿色为#01A187
  xlab("") + ylab("Consistency") + # 设置坐标轴标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # 横坐标标签倾斜45度
        axis.title.y = element_text(size = 14, face = "bold"), # 加粗纵坐标标题
        plot.title = element_text(hjust = 0.5), # 居中标题
        legend.position = "none", # 移除图例
        panel.grid.major = element_blank(), # 移除主要网格线
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.background = element_rect(fill = "white", colour = "grey50")) # 白色背景
print(p)

#去除异常值，作图HCconsistency
data <- data.frame(
  S07242 = c(0.77, 0.72, 0.35, 0.72, 0.62, 0.90, 0.71, 0.89),
  S07262 = c(0.85, 0.69, 0.78, 0.80, 0.92, 0.71, 0.42, 0.55),
  S07302 = c(0.57, 0.59, 0.39, 0.16, 0.31, 0.54, 0.85, 0.90),
  S08073 = c(0.92, 0.85, 0.84, 0.83, 0.84, 0.85, 0.84, 0.88),
  S08102 = c(0.84, 0.68, 0.48, 0.65, 0.62, 0.89, 0.81, 0.77),
  S08111 = c(0.95, 0.95, 0.70, 0.72, 0.88, 0.79, 0.82, 0.55),
  S08162 = c(0.56, 0.62, 0.18, 0.36, 0.51, 0.71, 0.50, 0.33),
  S09031 = c(0.83, 0.91, 0.74, 0.42, 0.56, 0.62, 0.47, 0.20),
  S09241 = c(0.88, 0.91, 0.88, 0.83, 0.78, 0.89, 0.89, 0.93),
  S09242 = c(0.82, 0.82, 0.85, 0.88, 0.16, 0.27, 0.86, 0.51),
  S09244 = c(0.82, 0.80, 0.57, 0.66, 0.34, 0.68, 0.40, 0.29),
  S09246 = c(0.31, 0.14, 0.73, 0.83, 0.46, 0.73, 0.64, 0.62),
  S08051 = c(0.33, 0.58, 0.71, 0.81, 0.84, 0.41, 0.62, 0.86),
  S08072 = c(0.63, 0.91, 0.90, 0.61, 0.83, 0.67, 0.43, 0.74))
statistic_raw <- matrix(nrow = ncol(data), ncol = 2) #原始数据的均值方差
for (j in 1:ncol(data)) {
  statistic_raw[j, ] <- c(mean(data[, j], na.rm = TRUE), var(data[, j], na.rm = TRUE))}
statistic <- matrix(nrow = ncol(data), ncol = 2)
colnames(statistic) <- c("Mean", "Variance")
for (i in 1:ncol(data)) {
  current_data <- data[, i]
  threshold <- 3 * mad(current_data, constant = 1)  # 使用constant = 1来得到原始MAD,定义异常值的阈值，N倍的MAD
  abs_devs <- abs(current_data - median(current_data, na.rm = TRUE))
  non_outliers_data <- current_data[which(abs_devs <= threshold)]
  data[setdiff(1:nrow(data), which(abs_devs <= threshold)), i] <- NA
  statistic[i, ] <- c(mean(non_outliers_data, na.rm = TRUE), var(non_outliers_data, na.rm = TRUE))}

data_long <- melt(data)
p <- ggplot(data_long, aes(x = variable, y = value, fill = "all_boxes")) +
  geom_boxplot(width = 0.6, outlier.shape = NA, lwd = 0.8, fatten = 1.5) + # 调整箱子宽度和误差线粗细
  scale_fill_manual(values = c("all_boxes" = "#01A187"), guide = FALSE) + # 蓝色#4DBBD4  HC绿色为#01A187
  xlab("") + ylab("Consistency") + # 设置坐标轴标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # 横坐标标签倾斜45度
        axis.title.y = element_text(size = 14, face = "bold"), # 加粗纵坐标标题
        plot.title = element_text(hjust = 0.5), # 居中标题
        legend.position = "none", # 移除图例
        panel.grid.major = element_blank(), # 移除主要网格线
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.background = element_rect(fill = "white", colour = "grey50")) # 白色背景
print(p)



#去除异常值，作图SZstablity
data <- data.frame(
  S07221 = c(0.0349, 0.0816, 0.1039, 0.0935, 0.0919, 0.0862, 0.1094, 0.1351),
  S07291 = c(0.0855, 0.0596, 0.0534, 0.0968, 0.0319, 0.0976, 0.0642, 0.1387),
  S08032 = c(0.1165, 0.0976, 0.1050, 0.1068, 0.0907, 0.0988, 0.0939, 0.1228),
  S08173 = c(0.0684, 0.1071, 0.0998, 0.1161, 0.1390, 0.1152, 0.0843, 0.0949),
  S08261 = c(0.0616, 0.0914, 0.0817, 0.0907, 0.1070, 0.1228, 0.0815, 0.0873),
  S09151 = c(0.0980, 0.1175, 0.0951, 0.0656, 0.0716, 0.0956, 0.0529, 0.0942),
  S09173 = c(0.1067, 0.1307, 0.1473, 0.1153, 0.1525, 0.1347, 0.0825, 0.1486),
  S09191 = c(0.1108, 0.0914, 0.0996, 0.1229, 0.1336, 0.0991, 0.0991, 0.0979),
  S09271 = c(0.0750, 0.0884, 0.1047, 0.0885, 0.0847, 0.0895, 0.0708, 0.1639),
  S10221 = c(0.1363, 0.0884, 0.1440, 0.1631, 0.1503, 0.1257, 0.1545, 0.1569),
  S11151 = c(0.1102, 0.0822, 0.0736, 0.1351, 0.1388, 0.1022, 0.1209, 0.0917),
  S11161 = c(0.1346, 0.1472, 0.1289, 0.1137, 0.1494, 0.1123, 0.1193, 0.1299),
  S11162 = c(0.1553, 0.1400, 0.1611, 0.1478, 0.1502, 0.1348, 0.1247, 0.1310),
  S08031 = c(0.0976 ,0.1088 ,0.1354 ,0.0641 ,0.1155 ,0.0502 ,0.1430 ,0.1246 ),
  S09192 = c(0.0752 ,0.0771 ,0.0951 ,0.0953 ,0.0479 ,0.1042 ,0.1101 ,0.1031 ),
  S08171 = c(0.1073 ,0.1027 ,0.1436 ,0.0954 ,0.1342 ,0.1083 ,0.1351 ,0.1585 ),
  S09171 = c(0.1226 ,0.1718 ,0.1347 ,0.1256 ,0.1155 ,0.1244 ,0.1376 ,0.1417 ))
statistic_raw <- matrix(nrow = ncol(data), ncol = 2) #原始数据的均值方差
for (j in 1:ncol(data)) {
  statistic_raw[j, ] <- c(mean(data[, j], na.rm = TRUE), var(data[, j], na.rm = TRUE))}
statistic <- matrix(nrow = ncol(data), ncol = 2)
colnames(statistic) <- c("Mean", "Variance")
for (i in 1:ncol(data)) {
  current_data <- data[, i]
  threshold <- 3 * mad(current_data, constant = 1)  # 使用constant = 1来得到原始MAD,定义异常值的阈值，N倍的MAD
  abs_devs <- abs(current_data - median(current_data, na.rm = TRUE))
  non_outliers_data <- current_data[which(abs_devs <= threshold)]
  data[setdiff(1:nrow(data), which(abs_devs <= threshold)), i] <- NA
  statistic[i, ] <- c(mean(non_outliers_data, na.rm = TRUE), var(non_outliers_data, na.rm = TRUE))}

data_long <- melt(data)
data_long <- data_long %>%
  mutate(group = ifelse(variable %in% colnames(data)[1:13], "Group1", "Group2"))
p <- ggplot(data_long, aes(x = variable, y = value, fill = group)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, lwd = 0.8, fatten = 1.5) + # 调整箱子宽度和误差线粗细
  scale_fill_manual(values = c("Group1" = "#4DBBD4", "Group2" = "#E64A35"), guide = FALSE) + # 蓝色#4DBBD4  HC绿色为#01A187
  xlab("") + ylab("Stability") + # 设置坐标轴标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # 横坐标标签倾斜45度
        axis.title.y = element_text(size = 14, face = "bold"), # 加粗纵坐标标题
        plot.title = element_text(hjust = 0.5), # 居中标题
        legend.position = "none", # 移除图例
        panel.grid.major = element_blank(), # 移除主要网格线
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.background = element_rect(fill = "white", colour = "grey50")) # 白色背景
print(p)


#去除异常值，作图HCstablity
data <- data.frame(
  S07242 = c(0.0885, 0.0850, 0.0856, 0.0953, 0.0723, 0.0931, 0.0938, 0.0946),
  S07262 = c(0.1002, 0.1070, 0.1671, 0.1410, 0.1200, 0.1183, 0.1532, 0.1330),
  S07302 = c(0.1092, 0.1316, 0.1011, 0.1040, 0.1258, 0.1217, 0.1186, 0.1466),
  S08073 = c(0.1546, 0.1688, 0.1413, 0.1748, 0.1016, 0.1868, 0.2135, 0.1727),
  S08102 = c(0.1687, 0.1386, 0.1391, 0.1003, 0.1223, 0.1307, 0.1271, 0.1199),
  S08111 = c(0.1615, 0.1419, 0.0849, 0.0682, 0.1592, 0.1742, 0.2125, 0.0964),
  S08162 = c(0.0747, 0.0714, 0.0972, 0.1260, 0.0847, 0.1258, 0.0887, 0.0713),
  S09031 = c(0.1853, 0.1330, 0.1175, 0.1418, 0.1393, 0.1572, 0.0958, 0.1519),
  S09241 = c(0.1257, 0.1282, 0.1268, 0.1271, 0.1303, 0.1225, 0.1638, 0.1237),
  S09242 = c(0.1026, 0.1257, 0.1335, 0.1417, 0.1208, 0.1150, 0.1178, 0.1477),
  S09244 = c(0.1357, 0.1344, 0.1417, 0.1353, 0.1381, 0.1289, 0.0986, 0.1250),
  S09246 = c(0.1479, 0.1380, 0.1239, 0.1506, 0.1303, 0.1546, 0.1433, 0.1510),
  S08051 = c(0.1105, 0.1501, 0.1439, 0.1447, 0.1112, 0.1109, 0.1636, 0.0891),
  S08072 = c(0.0990, 0.0589, 0.1120, 0.0977, 0.1148, 0.1706, 0.1354, 0.1065))
statistic_raw <- matrix(nrow = ncol(data), ncol = 2) #原始数据的均值方差
for (j in 1:ncol(data)) {
  statistic_raw[j, ] <- c(mean(data[, j], na.rm = TRUE), var(data[, j], na.rm = TRUE))}
statistic <- matrix(nrow = ncol(data), ncol = 2)
colnames(statistic) <- c("Mean", "Variance")
for (i in 1:ncol(data)) {
  current_data <- data[, i]
  threshold <- 3 * mad(current_data, constant = 1)  # 使用constant = 1来得到原始MAD,定义异常值的阈值，N倍的MAD
  abs_devs <- abs(current_data - median(current_data, na.rm = TRUE))
  non_outliers_data <- current_data[which(abs_devs <= threshold)]
  data[setdiff(1:nrow(data), which(abs_devs <= threshold)), i] <- NA
  statistic[i, ] <- c(mean(non_outliers_data, na.rm = TRUE), var(non_outliers_data, na.rm = TRUE))}
data_long <- melt(data)
p <- ggplot(data_long, aes(x = variable, y = value, fill = "all_boxes")) +
  geom_boxplot(width = 0.6, outlier.shape = NA, lwd = 0.8, fatten = 1.5) + # 调整箱子宽度和误差线粗细
  scale_fill_manual(values = c("all_boxes" = "#01A187"), guide = FALSE) + # 设置统一的颜色
  xlab("") + ylab("Stability") + # 设置坐标轴标题
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # 横坐标标签倾斜45度
        axis.title.y = element_text(size = 14, face = "bold"), # 加粗纵坐标标题
        plot.title = element_text(hjust = 0.5), # 居中标题
        legend.position = "none", # 移除图例
        panel.grid.major = element_blank(), # 移除主要网格线
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.background = element_rect(fill = "white", colour = "grey50")) # 白色背景
print(p)


#下面是肖师姐做的图
library('ggplot2')
library(ggalt)
library(reshape2)
library(tidyr)
library(ggthemr)
library(stringr)
library(openxlsx)
library(ggthemes)


library(dplyr)
library(ggsignif)
library(ggpubr) # 继承ggplot语法
library(patchwork) # 拼图包
library(ggsci) #配色包

##Compare Global Mean measures by subgroup
filedir<-'D:/Research_project/Interperson_FNIRS/Behavior'
setwd(filedir)
data <- read.xlsx("Behavior_raw3.xlsx", na.strings = c("", "NA"))
data$Group<-as.factor(data$Group)

data_long <- data %>%
  pivot_longer(contains('_'), 
               names_to = "GM_condition", 
               values_to = "GM")
data_long$GM_condition<-as.factor(data_long$GM_condition)
#data_long$Group <- factor(data_long$Group, levels = c("Positive", "Negative","HC (Role B)"))
data_long$Group <- factor(data_long$Group, levels = c("Positive", "Negative_BA","Negative_WD","HC (Role B)"))


data_long$GM_condition <- factor(data_long$GM_condition, 
                                 levels = c("Consistency_1", "Consistency_2","Consistency_3",
                                            "Consistency_4","Consistency_5","Consistency_6",
                                            "Consistency_7","Consistency_8","Consistency_9",
                                            "Consistency_10","Consistency_Avg",
                                            "Stability_1", "Stability_2","Stability_3",
                                            "Stability_4","Stability_5","Stability_6",
                                            "Stability_7","Stability_8","Stability_9",
                                            "Stability_10","Stability_Avg"))

subgrp_col4<-c('Positive'='#ff008a',
              'Negative_BA'='#8600ff',
              "Negative_WD"='#0032ff',
              "HC (Role B)"='#ff9200')

subgrp_col3<-c('Positive'='#ff008a',
              'Negative'='#0032ff',
              "HC (Role B)"='#ff9200')

GM_labels<-c("Consistency_1"='Trail 1',
             'Consistency_2'='Trail 2',
             'Consistency_3'='Trail 3',
             'Consistency_4'='Trail 4',
             'Consistency_5'='Trail 5',
             'Consistency_6'='Trail 6',
             'Consistency_7'='Trail 7',
             'Consistency_8'='Trail 8',
             'Consistency_9'='Trail 9',
             'Consistency_10'='Trail 10',
             'Consistency_Avg'='Over Trails')

for (plotname in 'Behavior_raw_consistency3_Neg') {
  test_data<-data_long %>%
    filter(grepl("Con", GM_condition))  #%>%
  #filter(grepl("Neg", Group))
  p <- ggplot(test_data,aes(x=GM_condition,y=GM,fill=Group))+
    stat_boxplot(geom='errorbar',color="black",width=0.3,position = position_dodge(0.5)
    )+
    geom_boxplot(size=0.2,width=0.3,linetype=1,linewidth=0.5,position = position_dodge(0.5)
    )+
    scale_fill_manual(values=subgrp_col)+
    scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=c(0.2,0.4,0.6,0.8))+
    #scale_y_continuous(expand=c(0,0),limits=c(0.04,0.2),breaks=c(0.05,0.1,0.15,0.2))+
    scale_x_discrete('Condition',labels=str_wrap(GM_labels, width=10))+
    theme_classic()+
    theme(
      axis.text.x=element_text(angle = 30,hjust=1,family="sans",size=12,color='black',margin = margin(b=3)), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
      axis.text.y=element_text(hjust = 0.1,vjust=0.5,family="sans",size=8), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
      axis.title.y=element_text(family="sans",size = 12), #设置y轴标题的字体属性
      axis.title.x = element_blank(),
      axis.ticks.x=element_blank(),
      #legend.position = 'none',
      axis.ticks = element_line(linewidth = 0.5,linetype = 1 ,lineend = 'round'),
      axis.line = element_line(linewidth = 0.5,linetype = 1 ,lineend = 'round'),
      #aspect.ratio = 1,
    ) +
    ylab('Behavior_Consistency')#设置x轴和y轴的标题
  p
  ggthemr('pale')
  ggsave(paste(plotname,'.png',sep=''),width=15,height=3,units="in",plot=p)
}
