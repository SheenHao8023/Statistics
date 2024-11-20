library(ggprism)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

group1 <- 'HC'
group2 <- 'PS'
group3 <- 'NS'
group3.1 <- 'BA'
group3.2 <- 'WD'
mycolors <-c('#66c2a5', '#fc8d62','#8da0cb')
mycolors2 <-c('#66c2a5','#fc8d62','#e78ac3','#a6d854')

# simulation
emp_data <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638,
        0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655,
        0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 
        0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
exp_means <- c(mean(emp_data$exp[emp_data$group == "HC"]),mean(emp_data$exp[emp_data$group == "PS"]),mean(emp_data$exp[emp_data$group == "NS"]))
exp_sds <- c(sd(emp_data$exp[emp_data$group == "HC"]),sd(emp_data$exp[emp_data$group == "PS"]),sd(emp_data$exp[emp_data$group == "NS"]))
data <- read_excel("C:/Users/haox8/Desktop/ND.xlsx")
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "NS")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(IC), .groups = 'drop')
ggplot(data = data_summary, mapping = aes(x = group, y = mean_bar, fill = group)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0) +
  geom_point(aes(y = exp_means), size = 2, fill = "black") +
  geom_errorbar(aes(ymin = exp_means - exp_sds, ymax = exp_means + exp_sds), width = 0.3, size = 0.7) +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors) +
  labs(x = element_blank(), y = "IC")

# simulation
emp_data <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3,4)),
levels = c(group1,group2,group3)),
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674,
        0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614,
        0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738,
        0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
exp_means <- c(mean(emp_data$exp[emp_data$group == "HC"]),mean(emp_data$exp[emp_data$group == "PS"]),mean(emp_data$exp[emp_data$group == "NS"]))
exp_sds <- c(sd(emp_data$exp[emp_data$group == "HC"]),sd(emp_data$exp[emp_data$group == "PS"]),sd(emp_data$exp[emp_data$group == "NS"]))
data <- read_excel("C:/Users/haox8/Desktop/ND.xlsx")
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "NS")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(WS), .groups = 'drop')
ggplot(data = data_summary, mapping = aes(x = group, y = mean_bar, fill = group)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0) +
  geom_point(aes(y = exp_means), size = 2, fill = "black") +
  geom_errorbar(aes(ymin = exp_means - exp_sds, ymax = exp_means + exp_sds), width = 0.3, size = 0.7) +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors) +
  labs(x = element_blank(), y = "WS")


# simulation
emp_data <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.736740518696357, 0.718849142735551, 0.463212981412826, 0.814311576831261, 0.675254248541746, 0.753779164191638,
        0.365773844835089, 0.496924019273831, 0.853665483618762, 0.81992918283153, 0.468329846354709, 0.625123655700655,
        0.254162034711293, 0.522578528601808, 0.47415946082266, 0.63329344053583, 0.310927820260869, 0.349407367664736, 
        0.482001044489097, 0.258375291767164, 0.213138136393294, 0.325858950437347, 0.122370341619107, 0.238565279588251))
exp_means <- c(mean(emp_data$exp[emp_data$group == "HC"]),mean(emp_data$exp[emp_data$group == "PS"]),
               mean(emp_data$exp[emp_data$group == "BA"]),mean(emp_data$exp[emp_data$group == "WD"]))
exp_sds <- c(sd(emp_data$exp[emp_data$group == "HC"]),sd(emp_data$exp[emp_data$group == "PS"]),
             sd(emp_data$exp[emp_data$group == "BA"]),sd(emp_data$exp[emp_data$group == "WD"]))
data <- read_excel("C:/Users/haox8/Desktop/ND.xlsx")
data$group <- factor(data$group, levels = c("HC", "PS", "BA", "WD"))
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "BA", "WD")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(IC), .groups = 'drop')
ggplot(data = data_summary, mapping = aes(x = group, y = mean_bar, fill = group)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0) +
  geom_point(aes(y = exp_means), size = 2, fill = "black") +
  geom_errorbar(aes(ymin = exp_means - exp_sds, ymax = exp_means + exp_sds), width = 0.3, size = 0.7) +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2) +
  labs(x = element_blank(), y = "IC")



# simulation
emp_data <- data.frame(group = factor(c(rep(group1,12),rep(group2,8),rep(group3.1,2),rep(group3.2,2)),
levels = c(group1,group2,group3.1,group3.2)),
exp = c(0.202281279176387, 0.630912927170263, 0.493304878268097, 0.918978968429336, 0.587325895507046, 0.637496186087674,
        0.229037385500683, 0.69514316108911, 0.552119687441578, 0.575134538823462, 0.633376077265468, 0.736980635980614,
        0.266276389621375, 0.34467419855378, 0.193338418384778, 0.254210502557368, 0.177190082140837, 0.771078858722738,
        0.578044979758555, 0.709004680909732, 0.423485965362685, 0.248186837930031, 0.537642775384007, 0.588964079608917))
exp_means <- c(mean(emp_data$exp[emp_data$group == "HC"]),mean(emp_data$exp[emp_data$group == "PS"]),
               mean(emp_data$exp[emp_data$group == "BA"]),mean(emp_data$exp[emp_data$group == "WD"]))
exp_sds <- c(sd(emp_data$exp[emp_data$group == "HC"]),sd(emp_data$exp[emp_data$group == "PS"]),
             sd(emp_data$exp[emp_data$group == "BA"]),sd(emp_data$exp[emp_data$group == "WD"]))
data <- read_excel("C:/Users/haox8/Desktop/ND.xlsx")
data$group <- factor(data$group, levels = c("HC", "PS", "BA", "WD"))
data_summary <- data %>%
  filter(group %in% c("HC", "PS", "BA", "WD")) %>%
  group_by(group) %>%
  summarize(mean_bar = mean(WS), .groups = 'drop')
ggplot(data = data_summary, mapping = aes(x = group, y = mean_bar, fill = group)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, colour = "black", size = 0) +
  geom_point(aes(y = exp_means), size = 2, fill = "black") +
  geom_errorbar(aes(ymin = exp_means - exp_sds, ymax = exp_means + exp_sds), width = 0.3, size = 0.7) +
  theme_prism(axis_text_angle = 0) +
  theme(legend.direction = "vertical",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = mycolors2) +
  labs(x = element_blank(), y = "WS")