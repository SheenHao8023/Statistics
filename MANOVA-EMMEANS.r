library(readxl)
library(bruceR)
library(dplyr)
library(writexl)

file_path <- "C:/Users/ASUS/Desktop/WaveletCoherence.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[data =="NaN"] <- NA
data$Age_centered <- scale(data$Age, center = TRUE, scale = FALSE)[, , drop = FALSE]
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:5, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
write_xlsx(data, "C:/Users/ASUS/Desktop/GlobalMean.xlsx")


m = MANOVA(data, dvs = "C1FC01:C5FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered","GMrest"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/WaveletCoherence1.doc")  %>%
    EMMEANS("Group",by = c("Condition","FunctionalConnectivity"),
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/WaveletCoherence2.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/WaveletCoherence3.doc")   %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/WaveletCoherence4.doc") 
emmip(m, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "Coherence")
emmip(m, Group ~ Condition | FunctionalConnectivity, CIs=TRUE)

m2 = MANOVA(data, dvs = "C1FC01:C5FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/FC1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/FC2.doc") 
emmip(m2, Group ~ Condition, CIs=TRUE, xlab = "Condition",ylab = "Coherence")
emmip(m2, Group ~ Condition | FunctionalConnectivity, CIs=TRUE)

#resting state vs. hearing each other
file_path <- "C:/Users/ASUS/Desktop/RS-EO.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[data =="NaN"] <- NA
data$Age_centered <- scale(data$Age, center = TRUE, scale = FALSE)[, , drop = FALSE]
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:2, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)

m1 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_EO1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO2.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO3.doc") 
m2 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered", "GMrest"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_EO4.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO5.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO6.doc") 
m3 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered", "GMall"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_EO7.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO8.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_EO9.doc") 


#resting state vs. tasking state
file_path <- "C:/Users/ASUS/Desktop/RS-TS.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[data =="NaN"] <- NA
data$Age_centered <- scale(data$Age, center = TRUE, scale = FALSE)[, , drop = FALSE]
data <- data %>%
  mutate(across(starts_with("C"), ~ as.numeric(.)))
data$GMrest <- apply(data[, sprintf("C1FC%02d", 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
data$GMall <- apply(data[, sprintf("C%dFC%02d", rep(1:2, each = 66), 1:66)], MARGIN = 1, FUN = mean, na.rm = TRUE)
write_xlsx(data, "C:/Users/ASUS/Desktop/GlobalMean2.xlsx")


m1 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_TSO1.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO2.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO3.doc") 
m2 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered", "GMrest"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_TSO4.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO5.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO6.doc") 
m3 = MANOVA(data, dvs = "C1FC01:C2FC66", dvs.pattern = "C(.)FC(..)", between = "Group",
        within = c("Condition", "FunctionalConnectivity"), covariate = c("Gender", "Age_centered", "GMall"),
        ss.type = "III", sph.correction = "GG", aov.include = FALSE, digits = 3,
        file = "C:/Users/ASUS/Desktop/RS_TSO7.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "bonferroni",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO8.doc")  %>%
    EMMEANS(c("Group","Condition"), by = "FunctionalConnectivity",
            contrast = "pairwise",reverse = TRUE,p.adjust = "fdr",sd.pooled = NULL,
            model.type = "multivariate",digits = 3,
            file = "C:/Users/ASUS/Desktop/RS_TSO9.doc") 