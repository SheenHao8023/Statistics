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

dat1 <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638, 0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655, 0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
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
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
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
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638, 0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655, 0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
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
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
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
