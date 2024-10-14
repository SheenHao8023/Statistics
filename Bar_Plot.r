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
exp = c(0.891295154749194, 0.891295154749194, 0.671573067892476, 0.955642168144407, 0.85653352419471, 0.924273334645529, 0.582310459475222, 0.728507018058812, 1, 0.963250938022753, 0.706334698431293, 0.76039455552594, 0.50999459026626, 0.49450146587976, 0.739312878313529, 0.659555268448715, 0.79077502056279, 0.582310459475222, 0.69495504648345, 0.428501666183376, 0.335159281223602, 0.49450146587976, 0, 0.374136884529915))
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
exp = c(0.0151907868262064, 0.562465940194699, 0.437859180632637, 1, 0.557766480062882, 0.646891421218873, 0.043478069481821, 0.676567519773946, 0.518433210136502, 0.509957749464245, 0.610955181429821, 0.730043321742096, 0.0744844863987189, 0.182064264755749, 0.208923593968205, 0, 0.157785565235872, 0.752991374036722, 0.555411308002633, 0.70887749123695, 0.344692039673947, 0.0728681920221744, 0.479297818346648, 0.54950741520125))
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
exp = c(0.891295154749194, 0.891295154749194, 0.671573067892476, 0.955642168144407, 0.85653352419471, 0.924273334645529, 0.582310459475222, 0.728507018058812, 1, 0.963250938022753, 0.706334698431293, 0.76039455552594, 0.50999459026626, 0.49450146587976, 0.739312878313529, 0.659555268448715, 0.79077502056279, 0.582310459475222, 0.69495504648345, 0.428501666183376, 0.335159281223602, 0.49450146587976, 0, 0.374136884529915))
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
exp = c(0.0151907868262064, 0.562465940194699, 0.437859180632637, 1, 0.557766480062882, 0.646891421218873, 0.043478069481821, 0.676567519773946, 0.518433210136502, 0.509957749464245, 0.610955181429821, 0.730043321742096, 0.0744844863987189, 0.182064264755749, 0.208923593968205, 0, 0.157785565235872, 0.752991374036722, 0.555411308002633, 0.70887749123695, 0.344692039673947, 0.0728681920221744, 0.479297818346648, 0.54950741520125))
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
