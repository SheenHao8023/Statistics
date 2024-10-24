library(ggprism)
library(ggsci)
library(ggpubr)
library(ggplot2)

group1 <- 'HC'
group2 <- 'Positive'
group3 <- 'Negative'
group3.1 <- 'Blunted Affect'
group3.2 <- 'Withdrawal'
mycolors <-c('#66c2a5', '#fc8d62','#8da0cb')
mycolors2 <-c('#66c2a5','#fc8d62','#e78ac3','#a6d854')
symcolors <-c('#fc8d62','#8da0cb')
symcolors2 <-c('#fc8d62','#e78ac3','#a6d854')


#行为学结果图
dat1 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638, 0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655, 0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
p1 = ggplot(data = dat1, mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    # axis.line = element_line(color="gray", size=0.5),  # 轴线颜色和粗细
    # panel.grid.major = element_line(color="lightgray", size=0.2),  # 主网格线
    # panel.grid.minor = element_blank(),  # 次网格线
    plot.title = element_text(hjust = 0.5, size=16, face="bold")  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ 
  labs(x=element_blank(), y="Consistency") +
  ylim(0, 1)  # 设置纵坐标范围
p1  

dat2 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
p2 = ggplot(data = dat2, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") +  
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ 
  labs(x=element_blank(), y="Stability")
p2

dat3 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638, 0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655, 0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
p3 = ggplot(data = dat3, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar",width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ 
  labs(x=element_blank(), y="Consistency")
p3

dat4 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
p4 = ggplot(data = dat4, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x),
               geom = "errorbar",width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ 
  labs(x=element_blank(), y="Stability")
p4

#症状学结果图：两组
sym1 <- data.frame(
            group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),
            exp = c(59,32,80,73,91,33,63,54,64,56,36,71)
            )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(
    fun = "mean", 
    fun.max = function(x) mean(x) + sd(x), 
    fun.min = function(x) mean(x) - sd(x), 
    geom = "errorbar", width = 0.4, size = 0.9
  ) + 
  geom_jitter(size = 2, width = 0.2)+ 
  theme(
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    # axis.line = element_line(color="gray", size=0.5),  # 轴线颜色和粗细
    plot.title = element_text(hjust = 0.5, size=16, face="bold")
  ) + 
  theme_prism(axis_text_angle = 45)+ 
  # coord_cartesian(ylim = c(0, 1)) + # 设置纵坐标范围
  scale_fill_manual(values = symcolors)+ 
  labs(x=element_blank(), y="PANSS")
p1  

sym1 <- data.frame(
    group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),
    exp = c(12,9,24,30,28,8,14,17,13,12,7,14)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(
    fun = "mean", 
    fun.max = function(x) mean(x) + sd(x), 
    fun.min = function(x) mean(x) - sd(x), 
    geom = "errorbar", width = 0.4, size = 0.9
  ) + 
  geom_jitter(size = 2, width = 0.2)+ 
  theme(
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    # axis.line = element_line(color="gray", size=0.5),  # 轴线颜色和粗细
    plot.title = element_text(hjust = 0.5, size=16, face="bold")
  ) + 
  theme_prism(axis_text_angle = 45)+ 
  # coord_cartesian(ylim = c(0, 1)) + # 设置纵坐标范围
  scale_fill_manual(values = symcolors)+ 
  labs(x=element_blank(), y="PANSS Positive Subscale")
p1  

sym1 <- data.frame(
    group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),
    exp = c(11,7,10,10,12,7,7,9,12,10,11,17)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(
    fun = "mean", 
    fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), 
    fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), 
    geom = "errorbar", width = 0.4, size = 0.9
  ) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ 
  theme(
    legend.direction = "vertical",
    axis.text = element_text(size=12, color="black"),  # 轴文本大小和颜色
    axis.title = element_text(size=14, face="bold"),   # 轴标题大小和样式
    # axis.line = element_line(color="gray", size=0.5),  # 轴线颜色和粗细
    plot.title = element_text(hjust = 0.5, size=16, face="bold")
  ) + 
  theme_prism(axis_text_angle = 45)+ 
  # coord_cartesian(ylim = c(0, 1)) + # 设置纵坐标范围
  scale_fill_manual(values = symcolors)+ 
  labs(x=element_blank(), y="PANSS Negative Subscale")
p1  

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(0,1,0,1,2,0,NA,NA,1,0,NA,1)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="RSESE")
p1  

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,0,5,1,0,8,14,0,4,1,3,1)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="ISI")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,0,10,2,0,12,18,21,4,0,8,10)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="BDI")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(28,5,12,4,NA,1,25,24,22,9,4,16)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="HAMD")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,11,8,11,13,13,12,20,12,8,9,10)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="IRI Fantasy Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,14,9,12,23,16,13,28,12,9,17,17)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="IRI Empathic Concern Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,16,5,12,23,14,5,28,12,8,17,14)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="IRI Perspective Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,17,12,9,16,18,15,16,11,8,18,17)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="IRI Personal Distress Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,50,50,50,49,50,52,49,NA,50,NA,50)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+coord_cartesian(ylim = c(45, 55)) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="UCLA")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,6,30,14,0,28,50,10,5,2,13,14)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="LSAS Anxiety Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,6,33,27,8,22,52,4,0,1,31,31)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="LSAS Avoidance Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,0.50,0.46,0.46,0.54,0.71,0.68,0.50,0.32,0.07,0.54,0.54)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="SQLS Motivation&Energy Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,0.03,0.28,0.09,0.19,0.22,0.22,0.16,0.13,0,0.03,0.03)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="SQLS Symptom&Side-effect Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(NA,0.03,0.45,0.05,0.18,0.50,0.73,0.48,0.12,0,0.20,0.35)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="SQLS Psychosocial Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(192,228,202,318,NA,208,NA,NA,192,260,NA,252)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="Token Motor Task")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(50,50,50,39,NA,42,NA,NA,42,74,36,39)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="Digit Symbol Test")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(9,9,9,8,NA,8,NA,NA,10,7,9,6)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="DST Sequence")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(4,7,6,4,NA,4,NA,NA,4,4,5,5)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="DST Reverse")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3,4)), levels = c(group2,group3) ),exp = c(13,16,15,12,NA,12,NA,NA,14,11,14,11)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors)+ labs(x=element_blank(), y="DST Sum")
p1


#症状学结果图：三组
sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ),exp = c(59,32,80,73,91,33,63,54,64,56,36,71))
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+   theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"),  axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+ scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="PANSS")
p1  

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ),exp = c(12,9,24,30,28,8,14,17,13,12,7,14))
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+   theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"),  axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+ scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="PANSS Positive Subscale")
p1  

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ),exp = c(11,7,10,10,12,7,7,9,12,10,11,17))
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))  + stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+   theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"),  axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+ scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="PANSS Negative Subscale")
p1  
               
sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ),exp = c(0,1,0,1,2,0,NA,NA,1,0,NA,1)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="RSESE")
p1  

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,0,5,1,0,8,14,0,4,1,3,1)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="ISI")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,0,10,2,0,12,18,21,4,0,8,10)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="BDI")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(28,5,12,4,NA,1,25,24,22,9,4,16)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="HAMD")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,11,8,11,13,13,12,20,12,8,9,10)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="IRI Fantasy Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,14,9,12,23,16,13,28,12,9,17,17)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="IRI Empathic Concern Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,16,5,12,23,14,5,28,12,8,17,14)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="IRI Perspective Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,17,12,9,16,18,15,16,11,8,18,17)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="IRI Personal Distress Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,50,50,50,49,50,52,49,NA,50,NA,50)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)+coord_cartesian(ylim = c(45, 55)) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="UCLA")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,6,30,14,0,28,50,10,5,2,13,14)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="LSAS Anxiety Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,6,33,27,8,22,52,4,0,1,31,31)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="LSAS Avoidance Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,0.50,0.46,0.46,0.54,0.71,0.68,0.50,0.32,0.07,0.54,0.54)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="SQLS Motivation&Energy Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,0.03,0.28,0.09,0.19,0.22,0.22,0.16,0.13,0,0.03,0.03)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="SQLS Symptom&Side-effect Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(NA,0.03,0.45,0.05,0.18,0.50,0.73,0.48,0.12,0,0.20,0.35)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45) + coord_cartesian(ylim = c(0, 1)) +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="SQLS Psychosocial Subscale")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(192,228,202,318,NA,208,NA,NA,192,260,NA,252)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="Token Motor Task")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(50,50,50,39,NA,42,NA,NA,42,74,36,39)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="Digit Symbol Test")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(9,9,9,8,NA,8,NA,NA,10,7,9,6)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="DST Sequence")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(4,7,6,4,NA,4,NA,NA,4,4,5,5)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="DST Reverse")
p1

sym1 <- data.frame(group = factor ( c(rep(group2,8),rep(group3.1,2),rep(group3.2,2)), levels = c(group2,group3.1,group3.2) ) ,exp = c(13,16,15,12,NA,12,NA,NA,14,11,14,11)  )
p1 = ggplot(data = sym1, mapping = aes(x = factor(group), y = exp, fill = group))+stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE), fun.min = function(x) mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE), geom = "errorbar", width = 0.4, size = 0.9) + 
  geom_jitter(size = 2, width = 0.2, na.rm = TRUE)+ theme(legend.direction = "vertical",axis.text = element_text(size=12, color="black"), axis.title = element_text(size=14, face="bold"),  plot.title = element_text(hjust = 0.5, size=16, face="bold")) + 
  theme_prism(axis_text_angle = 45)  +  scale_fill_manual(values = symcolors2)+ labs(x=element_blank(), y="DST Sum")
p1

