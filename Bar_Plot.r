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

dat1 <- data.frame(group = factor(c(rep(group1,8),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.75862103499935	,0.535098488765915	,0.872521703479465	,0.765114054551054	,
        0.641524893289217,	1.00 	,0.723090832789067	,0.783685520832751	,
        0.00 	,0.644539475122542	,0.770736101616157	,0.161478126033288	,
        0.479848645405395	,0.854591082405583	,0.620768072301197	,0.272745919178676	,
        0.281786262156535	,0.373622265556401	,0.488618921253315	,0.0841630178248565))
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

dat2 <- data.frame(group = factor(c(rep(group1,8),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.549350947151343	,0.576967272440511	,1.00 	,0.609867416285303	,
        0.86280213227854	,0.582753712866928	,0.634771656224769	,0.842130187993677	,
        0.38248775916535	,0.391868348116987	,0.524054010905166	,0.681313840969476	,
        0.394160335952587	,0.558235624610633	,0.500940978647547	,0.682722228160229	,
        0.00 	,0.435411496980127	,0.468410167091778	,0.598392485403485))
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

dat3 <- data.frame(group = factor(c(rep(group1,8),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.75862103499935	,0.535098488765915	,0.872521703479465	,0.765114054551054	,
        0.641524893289217,	1.00 	,0.723090832789067	,0.783685520832751	,
        0.00 	,0.644539475122542	,0.770736101616157	,0.161478126033288	,
        0.479848645405395	,0.854591082405583	,0.620768072301197	,0.272745919178676	,
        0.281786262156535	,0.373622265556401	,0.488618921253315	,0.0841630178248565))
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

dat4 <- data.frame(group = factor(c(rep(group1,8),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.549350947151343	,0.576967272440511	,1.00 	,0.609867416285303	,
        0.86280213227854	,0.582753712866928	,0.634771656224769	,0.842130187993677	,
        0.38248775916535	,0.391868348116987	,0.524054010905166	,0.681313840969476	,
        0.394160335952587	,0.558235624610633	,0.500940978647547	,0.682722228160229	,
        0.00 	,0.435411496980127	,0.468410167091778	,0.598392485403485))
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
