library(ggprism)
library(ggsci)
library(ggpubr)
library(ggplot2)

group1 <- 'Positive'
group2 <- 'Negative'
group3 <- 'HC'
group2.1 <- 'Blunted Affect'
group2.2 <- 'Withdrawal'
mycolors <-c('#4DBBD4','#E64A35' ,'#01A187')
mycolors2 <-c('#4DBBD4','#C43D2E','#FF6B52','#01A187')
dat1 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.39,0.46,0.79,0.09,
                          0.68,0.88,0.10,0.16,
                          0.56,0.32,0.96,0.89,0.68,0.92,0.85,0.65))
dat2 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.87,0.36,0.57,0.45,0.33,0.29,0.18,0.32,0.88,0.25,0.61,0.86,0.52,0.85,0.84,0.84))
dat3 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.21,0.41,0.18,0.54,0.22,0.11,0.13,0.69,0.68,0.55,0.94,0.67,0.60,0.95,0.83,0.82))
dat4 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.08,0.45,0.69,0.58,0.30,0.34,0,0.31,0.84,0.75,1,0.45,0.05,0.78,0.94,0.56))
dat5 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.33,0.64,0.67,0.29,0.37,0.18,0.70,0,0.85,0.88,0.61,0.66,0.29,0.37,0.96,0.68))
dat6 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.04,0.76,0.83,0.36,0.34,0.49,0,0.72,0.96,0.86,0.85,0.61,0.49,0.54,0.08,0.29))
dat7 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.64,	0.56,	0.49	,0.31	,0.32,	0.40,	0.50	,0.15	,0.74,	0.42,	0.70,	0.94,	0.75,	0.65,	0.27,	0.71))
dat8 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.47,	0.73,	0.86,	0.29,	0.14,	0.69,	0.27,	0.10,	0.36,	0.62,	0.37,	0.87,	0.46,	0.42,	0.93,	0.33))
dat9 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.59,	0.41,	0.69,	0.39,	0.23,	0.76,	0.01,	0.36,	0.50,	0.89,	0.73,	0.77,	0.22,	0.06,	0.45,	0.18))
dat10 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.31,	0.29,	0.14,	0.12,	0.63,	0.84,	0,	0.29,	0.71,	0.73,	0.90,	0.40,	0.09,	0.20,	0.75,	0.61))
p1 = ggplot(data = dat2, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ #设置数据映射
  stat_summary(fun = "mean", #stat_summary用于对数据进行统计并画图，fun参数选择要进行的统计
               geom = "bar", #设置要画的图的类型，在ggplot中geom后缀里选择
               width = 0.7, #柱的宽度
               colour = "black", #柱子描边的颜色
               size = 0.9)+ #柱子描边的粗细
  geom_jitter(size = 2, #第二层添加抖动点图，设置点的大小
              width = 0.2)+ #设置抖动的宽度
  stat_summary(fun = "mean", #第三层添加误差线，中间为均数
               fun.max = function(x) mean(x) + sd(x), #顶端为均数+标准差
               fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", #选择要画的图：误差线
               width = 0.4, #误差线的宽度
               size = 0.9)+ #误差线的粗细
  theme_prism(axis_text_angle = 45)+ #选择ggprism包中的主题函数套用graphpad的风格，并设置x轴角度
  theme(legend.direction = "horizontal") +  # 将图例方向设置为水平
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ #设置柱子填充的配色
  labs(x=element_blank(), y="Mutual Consistency")
p1  


dat1 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.39,0.46,0.79,0.09,0.68,0.88,0.10,0.16,0.56,0.32,0.96,0.89,0.68,0.92,0.85,0.65))
dat2 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.87,0.36,0.57,0.45,0.33,0.29,0.18,0.32,0.88,0.25,0.61,0.86,0.52,0.85,0.84,0.84))
dat3 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.21,0.41,0.18,0.54,0.22,0.11,0.13,0.69,0.68,0.55,0.94,0.67,0.60,0.95,0.83,0.82))
dat4 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.08,0.45,0.69,0.58,0.30,0.34,0,0.31,0.84,0.75,1,0.45,0.05,0.78,0.94,0.56))
dat5 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.33,0.64,0.67,0.29,0.37,0.18,0.70,0,0.85,0.88,0.61,0.66,0.29,0.37,0.96,0.68))
dat6 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.04,0.76,0.83,0.36,0.34,0.49,0,0.72,0.96,0.86,0.85,0.61,0.49,0.54,0.08,0.29))
dat7 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.64,	0.56,	0.49	,0.31	,0.32,	0.40,	0.50	,0.15	,0.74,	0.42,	0.70,	0.94,	0.75,	0.65,	0.27,	0.71))
dat8 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.47,	0.73,	0.86,	0.29,	0.14,	0.69,	0.27,	0.10,	0.36,	0.62,	0.37,	0.87,	0.46,	0.42,	0.93,	0.33))
dat9 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.59,	0.41,	0.69,	0.39,	0.23,	0.76,	0.01,	0.36,	0.50,	0.89,	0.73,	0.77,	0.22,	0.06,	0.45,	0.18))
dat10 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.31,	0.29,	0.14,	0.12,	0.63,	0.84,	0,	0.29,	0.71,	0.73,	0.90,	0.40,	0.09,	0.20,	0.75,	0.61))
p2 = ggplot(data = dat1, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ #设置数据映射
  stat_summary(fun = "mean", #stat_summary用于对数据进行统计并画图，fun参数选择要进行的统计
               geom = "bar", #设置要画的图的类型，在ggplot中geom后缀里选择
               width = 0.7, #柱的宽度
               colour = "black", #柱子描边的颜色
               size = 0.9)+ #柱子描边的粗细
  geom_jitter(size = 2, #第二层添加抖动点图，设置点的大小
              width = 0.2)+ #设置抖动的宽度
  stat_summary(fun = "mean", #第三层添加误差线，中间为均数
               fun.max = function(x) mean(x) + sd(x), #顶端为均数+标准差
               fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", #选择要画的图：误差线
               width = 0.4, #误差线的宽度
               size = 0.9)+ #误差线的粗细
  theme_prism(axis_text_angle = 45)+ #选择ggprism包中的主题函数套用graphpad的风格，并设置x轴角度
  theme(legend.direction = "horizontal") +  # 将图例方向设置为水平
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ #设置柱子填充的配色
  labs(x=element_blank(), y="Mutual Consistency")
p2

dat1 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.53,	0.50,	0.73,	1,	0.37,	0.96,	0.65,	0.75,	0.60,	0.38,	0.47,	0.64,	0.49,	0.64,	0.59,	0.62))
dat2 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.51,	0.17,	0.47,	0.75,	0.39,	0.25,	0.45,	0.54,	0.38,	0.46,	0.37,	0.88,	0.20,	1,	0.40,	0.64))
dat3 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.36,	0.31,	0.31,	0.70,	0.45,	0.20,	0.40,	0.95,	0.42,	0.71,	0.09,	0.63,	0.18,	0.60,	0.55,	0.61))
dat4 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.44,	0.28,	0.41,	0.84,	0.66,	0.37,	0.71,	0.65,	0.78,	0.57,	0.28,	0.52,	0.14,	0.33,	0.47,	0.55))
dat5 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.56,	0.47,	0.64,	0.77,	0.33,	0.49,	0.49,	0.66,	0.70,	0.73,	0.32,	0.34,	0.57,	0.71,	0.71,	0.65))
dat6 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.28,	0.35,	0.48,	0.56,	0.39,	0.07,	0.48,	0.39,	0.48,	0.42,	0.45,	0.50,	0.23,	0.63,	0.49,	0.62))
dat7 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.39,	0.52,	0.40,	0.59,	0.13,	0.42,	0.45,	0.53,	0.41,	0.35,	0.86,	0.52,	0.48,	0.74,	0.38,	0.50))
dat8 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.37,	0.30,	0.40,	0.55,	0.65,	0.46,	0.61,	0.62,	0.68,	0.74,	0.59,	0.54,	0.34,	0.38,	0.49,	0.39))
dat9 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.52,	0.30,	0.36,	0.57,	0.53,	0.40,	0.75,	0.64,	0.65,	0.37,	0.48,	0.56,	0.26,	0.76,	0.74,	0.60))
dat10 <- data.frame(group = factor(c(rep(group1,4),rep(group2,4),rep(group3,8)),
levels = c(group1,group2,group3)),
                  exp = c(0.34,	0.25,	0.35,	0.69,	0.36,	0.40,	0.39,	0.52,	0.25,	0.32,	0.33,	0.45,	0,	0.59,	0.69,	0.73))
p3 = ggplot(data = dat1, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ #设置数据映射
  stat_summary(fun = "mean", #stat_summary用于对数据进行统计并画图，fun参数选择要进行的统计
               geom = "bar", #设置要画的图的类型，在ggplot中geom后缀里选择
               width = 0.7, #柱的宽度
               colour = "black", #柱子描边的颜色
               size = 0.9)+ #柱子描边的粗细
  geom_jitter(size = 2, #第二层添加抖动点图，设置点的大小
              width = 0.2)+ #设置抖动的宽度
  stat_summary(fun = "mean", #第三层添加误差线，中间为均数
               fun.max = function(x) mean(x) + sd(x), #顶端为均数+标准差
               fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", #选择要画的图：误差线
               width = 0.4, #误差线的宽度
               size = 0.9)+ #误差线的粗细
  theme_prism(axis_text_angle = 45)+ #选择ggprism包中的主题函数套用graphpad的风格，并设置x轴角度
  theme(legend.direction = "horizontal") +  # 将图例方向设置为水平
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ #设置柱子填充的配色
  labs(x=element_blank(), y="Self Accuracy")
p3


dat1 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.53,	0.50,	0.73,	1,	0.37,	0.96,	0.65,	0.75,	0.60,	0.38,	0.47,	0.64,	0.49,	0.64,	0.59,	0.62))
dat2 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.51,	0.17,	0.47,	0.75,	0.39,	0.25,	0.45,	0.54,	0.38,	0.46,	0.37,	0.88,	0.20,	1,	0.40,	0.64))
dat3 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.36,	0.31,	0.31,	0.70,	0.45,	0.20,	0.40,	0.95,	0.42,	0.71,	0.09,	0.63,	0.18,	0.60,	0.55,	0.61))
dat4 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.44,	0.28,	0.41,	0.84,	0.66,	0.37,	0.71,	0.65,	0.78,	0.57,	0.28,	0.52,	0.14,	0.33,	0.47,	0.55))
dat5 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.56,	0.47,	0.64,	0.77,	0.33,	0.49,	0.49,	0.66,	0.70,	0.73,	0.32,	0.34,	0.57,	0.71,	0.71,	0.65))
dat6 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.28,	0.35,	0.48,	0.56,	0.39,	0.07,	0.48,	0.39,	0.48,	0.42,	0.45,	0.50,	0.23,	0.63,	0.49,	0.62))
dat7 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.39,	0.52,	0.40,	0.59,	0.13,	0.42,	0.45,	0.53,	0.41,	0.35,	0.86,	0.52,	0.48,	0.74,	0.38,	0.50))
dat8 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.37,	0.30,	0.40,	0.55,	0.65,	0.46,	0.61,	0.62,	0.68,	0.74,	0.59,	0.54,	0.34,	0.38,	0.49,	0.39))
dat9 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.52,	0.30,	0.36,	0.57,	0.53,	0.40,	0.75,	0.64,	0.65,	0.37,	0.48,	0.56,	0.26,	0.76,	0.74,	0.60))
dat10 <- data.frame(group = factor(c(rep(group1,4),rep(group2.1,2),rep(group2.2,2),rep(group3,8)),
levels = c(group1,group2.1,group2.2,group3)),
                  exp = c(0.34,	0.25,	0.35,	0.69,	0.36,	0.40,	0.39,	0.52,	0.25,	0.32,	0.33,	0.45,	0,	0.59,	0.69,	0.73))
p4 = ggplot(data = dat1, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ #设置数据映射
  stat_summary(fun = "mean", #stat_summary用于对数据进行统计并画图，fun参数选择要进行的统计
               geom = "bar", #设置要画的图的类型，在ggplot中geom后缀里选择
               width = 0.7, #柱的宽度
               colour = "black", #柱子描边的颜色
               size = 0.9)+ #柱子描边的粗细
  geom_jitter(size = 2, #第二层添加抖动点图，设置点的大小
              width = 0.2)+ #设置抖动的宽度
  stat_summary(fun = "mean", #第三层添加误差线，中间为均数
               fun.max = function(x) mean(x) + sd(x), #顶端为均数+标准差
               fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", #选择要画的图：误差线
               width = 0.4, #误差线的宽度
               size = 0.9)+ #误差线的粗细
  theme_prism(axis_text_angle = 45)+ #选择ggprism包中的主题函数套用graphpad的风格，并设置x轴角度
  theme(legend.direction = "horizontal") +  # 将图例方向设置为水平
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ #设置柱子填充的配色
  labs(x=element_blank(), y="Self Accuracy")
p4


#基于算好的均值标准差直接作图
# 创建示例数据
mycolors <-c('#4DBBD4','#E64A35' )
datafig <- data.frame(
  group = rep(c('Resting', 'Hearing self'), each=1),
  mean_value = c(0.553, 0.521),  
  sd_value = c(0.097,0.121)    
)

# 绘制图形
p1 <- ggplot(data = datafig, aes(x = factor(group), y = mean_value, fill = group)) +
  geom_bar(stat = "identity", width = 0.7, colour = "black", size = 0.9) +  # 使用身份统计，因为y轴是直接从数据中来的
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),  # 添加误差线
                width = 0.4, size = 0.9) +
  theme_prism()+
  theme(axis.text.x = element_text(angle = 45)) +  # 注意这里使用了element_text代替了axis_text_angle
  theme(legend.direction = "horizontal") +  # 将图例放置在底部
  coord_cartesian(ylim = c(0, 0.8)) +
  scale_fill_manual(values = mycolors) +
  labs(x = "Condition", y = "Coherence")
p1





###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# 数据准备 - 为方便指定颜色，增加一列Color来区分A/C与B/D
data <- data.frame(
  Group = c("Positive_MC", "Positive_SA", "Negative_MC", "Negative_SA", "Healthy_MC", "Healthy_SA"),
  Value = c(0.55, 0.49, 0.30, 0.49, 0.59, 0.52),
  Value2 = c(0.14, 0.13, 0.17, 0.16, 0.14, 0.13),
  Color = c("AC_color", "BD_color", "AC_color", "BD_color", "AC_color", "BD_color")
)

# 创建柱形图
ggplot(data, aes(x = Group, y = Value, fill = Color)) +
  geom_col(position = "dodge", color = "black", size = 0.5, width = 0.3) +
  #geom_errorbar(aes(ymin = Value - Value2, ymax = Value + Value2), position = position_dodge(width = 0.3), width = 0.2, color = "black") +
  scale_fill_manual(values = c("AC_color" = "#33A1C9", "BD_color" = "#E74C3C")) + # 自定义颜色
  theme_minimal() +
  labs(title = "MC and SA value across the group", x = "Groups", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 0.8)) 

# 创建柱形图
ggplot(data, aes(x = Group, y = Value2, fill = Color)) +
  geom_col(position = "dodge", color = "black", size = 0.5, width = 0.3) +
  scale_fill_manual(values = c("AC_color" = "#33A1C9", "BD_color" = "#E74C3C")) + # 自定义颜色
  theme_minimal() +
  labs(title = "Self Accuracy", x = "Subjects", y = "Self Accuracy") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") 

library(readxl)
library(tidyr)
library(ggplot2)
file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/check_Lexi/Behavior.xlsx"
data <- read_excel(file_path, col_names = TRUE)
df_long <- pivot_longer(data, cols = c(`Mutual Consistency`, `Self Accuracy`), names_to = "Indicator", values_to = "Value")
ggplot(df_long, aes(x = Subject, y = Value, fill = Indicator)) +
  geom_col(position = position_dodge(width = 0.9, preserve = "single"), width = 0.4) + # 使用geom_col并设置柱子位置 dodge（分散）
  theme(axis.text.x = element_text(size = 18, color = "black"), # 设置x轴标签的字体大小和颜色
        axis.text.y = element_text(size = 18, color = "black")) +
  scale_fill_manual(values = c("blue", "red"), # 自定义颜色
                    name = "Indicators", # 图例标题
                    labels = c("Mutual Consistency", "Self Accuracy")) + # 图例标签
  labs(title = "Comparison of MC and SA across Subjects", # 图表标题
       x = "Subject", # x轴标签
       y = "Score") + # y轴标签
  theme_minimal() + # 使用简约主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 倾斜x轴标签以避免重叠

