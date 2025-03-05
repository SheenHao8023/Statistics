library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

file_path <- "C:/Users/haox8/Desktop/tES_SZ_Behav/symptom_data.xlsx"
data <- read_excel(file_path)
long_data <- data %>% 
  #  filter(Group == 1) %>% # Experimental
  #  filter(Group == 2) %>% # ControlActive
  #  filter(Group == 3) %>% # ControlSham
  #  filter(Group == 4) %>% # ControlResting
  filter(ID == 'HZ002' & Type == 'SZ') %>% # Individual
  pivot_longer(cols = 9:ncol(data), names_to = "Measure", values_to = "Score")
time_levels <- sort(unique(long_data$Time))
long_data$Time <- factor(long_data$Time, levels = time_levels)
theme_custom <- theme(
  panel.grid = element_blank(),
  axis.line = element_line(color = 'black', size = 0.75),
  axis.ticks = element_line(color = 'black', size = 0.75),
  axis.text = element_text(size = 12, color = "black"),
  axis.title = element_text(size = 14),
  plot.title = element_text(size = 18, face = "bold"),
  legend.position = "none"
)
color_map <- c("Pre" = "#f47c7c", "Post" = "#80d6ff", "FollowUp1" = "#92d050", "FollowUp2" = "#ffc000")
measure_names <- unique(long_data$Measure) 

for (measure in measure_names) {
  p <- ggplot(long_data %>% filter(Measure == measure), aes(x = Time, y = Score, fill = Time)) +
    geom_bar(stat = "summary", fun = "mean", width = 0.6, colour = "black", size = 0.5) +
    geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, size = 0.8) +
    scale_fill_manual(values = color_map[names(color_map) %in% time_levels]) +
    labs(x = "Time", y = "Score", title = measure) +
    theme_custom
  output_path <- paste0(dirname(file_path), "/", measure, "_bar_plot.png")
  ggsave(output_path, plot = p, width = 4, height = 6, dpi = 300)
}
