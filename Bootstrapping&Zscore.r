library(boot)
library(readxl)
library(writexl)

file_path <- "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/11Behavior_brain.xlsx"
data <- read_excel(file_path, col_names = TRUE)
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)}  # 检查并均值替换每一列的缺失值
  return(x)
})
my_mean_function <- function(data, indices) {return(mean(data[indices]))}
Zdata <- data.frame(matrix(ncol = ncol(data), nrow = 1000))  
colnames(Zdata) <- colnames(data)  # 复制列名

set.seed(8023)
results <- boot(data$mutual_consistency, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,2] <- results$t

set.seed(8023)
results <- boot(data$self_accuracy, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,3] <- results$t

set.seed(8023)
results <- boot(data$RA_RTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,7] <- results$t

set.seed(8023)
results <- boot(data$RA_RM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,8] <- results$t

set.seed(8023)
results <- boot(data$RA_RDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,9] <- results$t

set.seed(8023)
results <- boot(data$RA_RSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,10] <- results$t

set.seed(8023)
results <- boot(data$RA_RFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,11] <- results$t

set.seed(8023)
results <- boot(data$RA_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,12] <- results$t

set.seed(8023)
results <- boot(data$RA_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,13] <- results$t

set.seed(8023)
results <- boot(data$RA_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,14] <- results$t

set.seed(8023)
results <- boot(data$RA_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,15] <- results$t

set.seed(8023)
results <- boot(data$RA_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,16] <- results$t

set.seed(8023)
results <- boot(data$RA_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,17] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_RM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,18] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_RDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,19] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_RSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,20] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_RFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,21] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,22] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,23] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,24] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,25] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,26] <- results$t

set.seed(8023)
results <- boot(data$RTPJ_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,27] <- results$t

set.seed(8023)
results <- boot(data$RM_RDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,28] <- results$t

set.seed(8023)
results <- boot(data$RM_RSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,29] <- results$t

set.seed(8023)
results <- boot(data$RM_RFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,30] <- results$t

set.seed(8023)
results <- boot(data$RM_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,31] <- results$t

set.seed(8023)
results <- boot(data$RM_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,32] <- results$t

set.seed(8023)
results <- boot(data$RM_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,33] <- results$t

set.seed(8023)
results <- boot(data$RM_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,34] <- results$t

set.seed(8023)
results <- boot(data$RM_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,35] <- results$t

set.seed(8023)
results <- boot(data$RM_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,36] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_RSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,37] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_RFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,38] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,39] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,40] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,41] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,42] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,43] <- results$t

set.seed(8023)
results <- boot(data$RDLPFC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,44] <- results$t

set.seed(8023)
results <- boot(data$RSFC_RFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,45] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,46] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,47] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,48] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,49] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,50] <- results$t

set.seed(8023)
results <- boot(data$RSFC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,51] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LFPC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,52] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,53] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,54] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,55] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,56] <- results$t

set.seed(8023)
results <- boot(data$RFPC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,57] <- results$t

set.seed(8023)
results <- boot(data$LFPC_LSFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,58] <- results$t

set.seed(8023)
results <- boot(data$LFPC_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,59] <- results$t

set.seed(8023)
results <- boot(data$LFPC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,60] <- results$t

set.seed(8023)
results <- boot(data$LFPC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,61] <- results$t

set.seed(8023)
results <- boot(data$LFPC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,62] <- results$t

set.seed(8023)
results <- boot(data$LSFC_LDLPFC, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,63] <- results$t

set.seed(8023)
results <- boot(data$LSFC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,64] <- results$t

set.seed(8023)
results <- boot(data$LSFC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,65] <- results$t

set.seed(8023)
results <- boot(data$LSFC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,66] <- results$t

set.seed(8023)
results <- boot(data$LDLPFC_LM, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,67] <- results$t

set.seed(8023)
results <- boot(data$LDLPFC_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,68] <- results$t

set.seed(8023)
results <- boot(data$LDLPFC_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,69] <- results$t

set.seed(8023)
results <- boot(data$LM_LTPJ, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,70] <- results$t

set.seed(8023)
results <- boot(data$LM_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,71] <- results$t

set.seed(8023)
results <- boot(data$LTPJ_LA, statistic = my_mean_function, R = 1000)
ci <- boot.ci(results, type = "perc") 
Zdata[,72] <- results$t

write_xlsx(Zdata, "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/nonZdata.xlsx")

Zdata2 <- data.frame(matrix(ncol = ncol(data), nrow = 1000))  
colnames(Zdata2) <- colnames(data)  # 复制列名
for (columnName in names(Zdata)) {
  Zdata2[[columnName]] <- scale(Zdata[[columnName]])} # 对数据框中的每一列进行Z分数转换，并替换原值
write_xlsx(Zdata2, "C:/Users/ASUS/Desktop/AI_SCZ/Analysis_combine/Zdata2.xlsx")
