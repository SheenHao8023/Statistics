library(ggprism)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# empirical data
emp_data <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WD')),
  exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638,
          0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655,
          0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 
          0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251,
          0, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
emp_summary <- emp_data %>%
  group_by(group) %>%
  summarise(Mean = mean(exp, na.rm=TRUE), SD=sd(exp, na.rm=TRUE))
emp_summary[4, 3] = 0
emp_summary$group <- factor(emp_summary$group, levels = c("HC", "PS", "NS", 'NA', "BA", "WD"))
#Single shot simulation
data <- read_excel("C:/Users/ASUS/Desktop/SS.xlsx")
data$group <- factor(data$group, levels = c("HC", "PS", "NS", "BA", "WD"))
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "NS", "BA", "WD")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(IC), .groups = 'drop')
data_summary <- rbind(data_summary[1:3, ], data.frame(group='NA', mean_bar=0), data_summary[4:5, ] )
data_summary$group <- factor(data_summary$group, levels = c("HC", "PS", "NS", 'NA', "BA", "WD"))
#plot
ggplot(data = data_summary, aes(x=group, y=mean_bar, fill=group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, alpha=1) + 
  geom_point(data = subset(emp_summary, group!= 'NA'), aes(x = group, y = Mean, fill = group), size = 1) +
  geom_errorbar(data = subset(emp_summary, group!= 'NA'), aes(x = group, ymin = Mean-SD, ymax =Mean+SD), inherit.aes=FALSE, width = 0.4, size = 0.7, color = "black") +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c("HC", "PS", "NS", "BA", "WD"), labels=c("HC", "PS", "NS", "BA", "WD"))+
  labs(x=NULL, y="IC")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.4), fill = NA, color='black', linetype = 'dashed', size=0.75)


# WS value
emp_data <- data.frame(group = factor(c(rep('HC',12),rep('PS',8),rep('NS',4),rep('NA',1),rep('BA',2),rep('WD',2)),
  levels = c('HC','PS','NS','NA','BA','WD')),
  exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674,
        0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614,
        0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738,
        0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917,
        0,  0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
emp_summary <- emp_data %>%
  group_by(group) %>%
  summarise(Mean = mean(exp, na.rm=TRUE), SD=sd(exp, na.rm=TRUE))
emp_summary[4, 3] = 0
emp_summary$group <- factor(emp_summary$group, levels = c("HC", "PS", "NS", 'NA', "BA", "WD"))
data <- read_excel("C:/Users/ASUS/Desktop/SS.xlsx")
data$group <- factor(data$group, levels = c("HC", "PS", "NS", "BA", "WD"))
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "NS", "BA", "WD")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(WS), .groups = 'drop')
data_summary <- rbind(data_summary[1:3, ], data.frame(group='NA', mean_bar=0), data_summary[4:5, ] )
data_summary$group <- factor(data_summary$group, levels = c("HC", "PS", "NS", 'NA', "BA", "WD"))
ggplot(data = data_summary, aes(x=group, y=mean_bar, fill=group)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, alpha=1) + 
  geom_point(data = subset(emp_summary, group!= 'NA'), aes(x = group, y = Mean, fill = group), size = 1) +
  geom_errorbar(data = subset(emp_summary, group!= 'NA'), aes(x = group, ymin = Mean-SD, ymax =Mean+SD), inherit.aes=FALSE, width = 0.4, size = 0.7, color = "black") +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
    axis.line = element_line(color = 'black', size = 0.75),
    axis.ticks = element_line(color = 'black', size = 0.75),
    axis.text = element_text(size=12, color="black", face='plain'),  
    axis.title = element_text(size=14, face='plain')) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb', 'transparent', '#e78ac3','#a6d854'))+ 
  scale_x_discrete(breaks = c("HC", "PS", "NS", "BA", "WD"), labels=c("HC", "PS", "NS", "BA", "WD"))+
  labs(x=NULL, y="WS")+
  geom_rect(aes(xmin=2.5, xmax=3.5,ymin=-0.02,ymax=0.7), fill = NA, color='black', linetype = 'dashed', size=0.75)
