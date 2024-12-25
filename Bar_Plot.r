library(ggprism)
library(ggsci)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)

#症状学结果
data <- data.frame( 
  "A" = c(0.084745763,0.09375,0.075,0.136986301,0.065934066,0.090909091,0.095238095,0.12962963,0.125,0.053571429,0.055555556,0.070422535,0,0.125,0.053571429,0.055555556,0.070422535),
  "B" = c(0.06779661,0.0625,0.025,0.02739726,0.021978022,0.060606061,0.031746032,0.037037037,0.03125,0.035714286,0.111111111,0.112676056,0,0.03125,0.035714286,0.111111111,0.112676056),
  "C" = c(0.06779661,0.0625,0.025,0.02739726,0.021978022,0.060606061,0.031746032,0.074074074,0.125,0.071428571,0.111111111,0.084507042,0,0.125,0.071428571,0.111111111,0.084507042))
data$Group <- c(rep("PS", 8), rep("NS", 4), rep('NA', 1), rep("BA", 2), rep("WD", 2))
data_long <- data %>%
  pivot_longer(cols = -Group, names_to = "Metric", values_to = "Value")
summary_data <- data_long %>%
  group_by(Metric, Group) %>%
  summarise(mean = mean(Value), sd = sd(Value), .groups = 'drop')
levels(summary_data$Metric) <- c("A", "B", "C")
summary_data$Group <- factor(summary_data$Group, levels = c('PS','NS','NA','BA','WD'))
data_long$Group <- factor(data_long$Group, levels = c('PS','NS','NA','BA','WD'))
summary_data$Metric <- factor(summary_data$Metric, levels = c("A", "B", "C"))
summary_data$sd[is.na(summary_data$sd)] <- 0
ggplot(summary_data, aes(x = Group, y = mean, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7, colour = NA) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.4, position = position_dodge(width = 0.9), linewidth = 0.7) + 
  geom_jitter(data = subset(data_long, Group != 'NA'), aes(x = Group, y = Value, color = Metric), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), alpha = 1, size = 1) + 
  scale_fill_manual(values = c('#fc8d62','#8da0cb','#6495ed'), name = "Items", labels = c("P1+P3", "N2+N4", "2N1")) + 
  scale_color_manual(values = c('#fc8d62','#8da0cb','#6495ed'), name = "Items") + 
  scale_x_discrete(breaks = c('PS','NS','BA','WD'), labels = c('PS', 'NS', 'BA', 'WD')) + 
  coord_cartesian(ylim = c(0, 0.15)) +
  labs(x = NULL, y = "Relative Scores") +
  theme_minimal() +
  theme(legend.direction = "vertical", legend.position = 'right',
        axis.line = element_line(color = 'black', size = 0.75),
        axis.ticks = element_line(color = 'black', size = 0.75),
        axis.text = element_text(size = 12, color = "black", face = 'plain'),  
        axis.title = element_text(size = 14, face = 'plain'))


#行为学结果IC
dat1 <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WS')),
  exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 
          0.675254248541746, 0.753779164191638, 0.365773844835089, 0.496924019273831, 
          0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655, 
          0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 
          0.310927820260869, 0.349407367664736, 0.482001044489097, 0.258375291767164, 
          0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251, 0,
          0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
ggplot(data = dat1, mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = NA, size = 0.9)+ 
  geom_jitter(data = subset(dat1, group!= group3.0), size = 1)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.7)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c('HC','PS','NS','BA','WS'), labels=c('HC','PS','NS','BA','WS'))+
  labs(x=NULL, y="IC")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.4), fill = NA, color='black', linetype = 'dashed', size=0.75)
#行为学结果WS
dat2 <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WS')),
  exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 
          0.587325895507046, 0.637496186087674, 0.229037385500683, 0.69514316108911, 
          0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614, 
          0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 
          0.177190082140837, 0.771078858722738, 0.578044979758555, 0.709004680909732, 
          0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917, 0,
          0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
ggplot(data = dat2, mapping = aes(x = factor(group), y = exp, fill = group))+ 
  stat_summary(fun = "mean",geom = "bar", width = 0.7, colour = NA, size = 0.9)+ 
  geom_jitter(data = subset(dat2, group!= group3.0), size = 1)+ 
  stat_summary(fun = "mean", fun.max = function(x) mean(x) + sd(x), fun.min = function(x) mean(x) - sd(x), 
               geom = "errorbar", width = 0.4, size = 0.7)+ 
  theme_prism(axis_text_angle = 0)+ 
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c('HC','PS','NS','BA','WS'), labels=c('HC','PS','NS','BA','WS'))+
  labs(x=NULL, y="WS")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.65), fill = NA, color='black', linetype = 'dashed', size=0.75)
