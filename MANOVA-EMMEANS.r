library(readxl)
library(bruceR)
library(dplyr)
library(writexl)

file_path <- "C:/Users/ASUS/Desktop/SCZ_tACS/data/IBS/statistic/IBS.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:5, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)

m1 = MANOVA(data, dvs = "C1FC01:C5FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/IBS1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS2.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS3.doc") 
emmip(m1, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m1, Group ~ Condition | IBS, CIs=TRUE)

#作图
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(ggprism)
data <- read_excel("C:/Users/haox8/Desktop/coherence.xlsx")
data_long <- pivot_longer(data, cols = c('C1FC35','C2FC35'), names_to = "Condition", values_to = "Coherence")
data_long$Group <- factor(data_long$Group, labels = c("SZ", "HC"))
data_long$Condition <- factor(data_long$Condition, labels = c("Resting state", "Tasking state"))
data_long$Coherence <- as.numeric(data_long$Coherence)
summary_data <- data_long %>%
  group_by(Condition, Group) %>%
  summarise(Mean = mean(Coherence, na.rm = TRUE), SD = sd(Coherence, na.rm = TRUE)) %>%
  ungroup()
ggplot(summary_data, aes(x = Condition, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), size = 0.7, width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Condition", y = "RDLPFC-LDLPFC", fill = "Group") +
  theme_minimal() +
  scale_fill_manual(values = c("#EE7D80","#D3D3D3")) +  # 设置颜色
  theme_prism(axis_text_angle = 0) + 
  coord_cartesian(ylim = c(0, 0.8)) +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12)) # 改变轴标签字体大小


m2 = MANOVA(data, dvs = "C1FC01:C5FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMrest",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/IBS4.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS5.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS6.doc") 
emmip(m2, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m2, Group ~ Condition | IBS, CIs=TRUE)

m3 = MANOVA(data, dvs = "C1FC01:C5FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMall",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/IBS7.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS8.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/IBS9.doc") 
emmip(m3, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m3, Group ~ Condition | IBS, CIs=TRUE)

file_path <- "C:/Users/haox8/Desktop/coherence.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[data == "NaN"] <- NA
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:2, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)

m1 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "Coherence"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/haox8/Desktop/RS-TS1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "Coherence",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/haox8/Desktop/RS-TS2.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "Coherence",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/haox8/Desktop/RS-TS3.doc") 
emmip(m1, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m1, Group ~ Condition | IBS, CIs=TRUE)

m2 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMrest",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-EO4.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO5.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO6.doc") 
emmip(m2, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m2, Group ~ Condition | IBS, CIs=TRUE)

m3 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMall",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-EO7.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO8.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO9.doc") 
emmip(m3, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m3, Group ~ Condition | IBS, CIs=TRUE)

file_path <- "C:/Users/ASUS/Desktop/SCZ_tACS/data/IBS/statistic/RS-TS.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:2, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)

m1 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-TS1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS2.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS3.doc") 
emmip(m1, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m1, Group ~ Condition | IBS, CIs=TRUE)

m2 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMrest",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-TS4.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS5.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS6.doc") 
emmip(m2, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m2, Group ~ Condition | IBS, CIs=TRUE)

m3 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), covariate = "GMall",
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-TS7.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS8.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-TS9.doc") 
emmip(m3, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "IBS")
emmip(m3, Group ~ Condition | IBS, CIs=TRUE)
