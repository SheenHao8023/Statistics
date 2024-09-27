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