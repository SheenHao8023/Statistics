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

file_path <- "C:/Users/ASUS/Desktop/SCZ_tACS/data/IBS/statistic/RS-EO.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:2, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)

m1 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "IBS"), 
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS-EO1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO2.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "IBS",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS-EO3.doc") 
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
