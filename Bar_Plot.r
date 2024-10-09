library(ggprism)
library(ggsci)
library(ggpubr)
library(ggplot2)

group1 <- 'HC'
group2 <- 'Positive'
group3 <- 'Negative'
group3.1 <- 'Blunted Affect'
group3.2 <- 'Withdrawal'
mycolors <-c('#01A187', '#4DBBD4','#E64A35')
mycolors2 <-c('#01A187','#4DBBD4','#B22222','#FF6B52')

dat1 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.746031746,0.746031746,0.476190476,0.984126984,0.761904762,0.888888889,0.365079365,0.555555556,1,0.650793651,0.523809524,0.507936508,0,
        0.26984127,0.571428571,0.46031746,0.571428571,0.365079365,0.507936508,0.253968254,0.095238095,0.26984127,0.015873016,0.158730159))
p1 = ggplot(data = dat1, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") +  
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ 
  labs(x=element_blank(), y="Consistency")
p1  

dat2 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.116550117,0.6002331,0.481351981,1,0.60955711,0.686480186,0.163170163,0.719114219,0.611888112,0.548951049,0.596736597,0.745920746,0,0.297202797,
        0.286713287,0.13986014,0.32983683,0.715617716,0.593240093,0.752913753,0.307692308,0.116550117,0.51981352,0.649184149))
p2 = ggplot(data = dat2, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar", width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") +  
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors)+ 
  labs(x=element_blank(), y="Stability")
p2

dat3 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.746031746,0.746031746,0.476190476,0.984126984,0.761904762,0.888888889,0.365079365,0.555555556,1,0.650793651,0.523809524,0.507936508,0,
        0.26984127,0.571428571,0.46031746,0.571428571,0.365079365,0.507936508,0.253968254,0.095238095,0.26984127,0.015873016,0.158730159))
p3 = ggplot(data = dat3, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar",width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ 
  labs(x=element_blank(), y="Consistency")
p3

dat4 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.116550117,0.6002331,0.481351981,1,0.60955711,0.686480186,0.163170163,0.719114219,0.611888112,0.548951049,0.596736597,0.745920746,0,0.297202797,
        0.286713287,0.13986014,0.32983683,0.715617716,0.593240093,0.752913753,0.307692308,0.116550117,0.51981352,0.649184149))
p4 = ggplot(data = dat4, 
            mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0.9)+ 
  geom_jitter(size = 2, width = 0.2)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), #底端为均数-标准差
               geom = "errorbar",width = 0.4, size = 0.9)+ 
  theme_prism(axis_text_angle = 45)+ 
  theme(legend.direction = "vertical") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2)+ 
  labs(x=element_blank(), y="Stability")
p4
