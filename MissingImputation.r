library(haven)
library(dplyr)
library(writexl)
data <- read_spss("C:\\Users\\ASUS\\Desktop\\SCZ_tACS\\ROI\\GrangerCausality\\GCvalue\\MANOVA_GCA.sav")  # 读取.sav文件
cols_to_fill <- names(data)[5:664] # 定义一个筛选函数，特定列
data_filled <- data %>%   # 对于指定列，按照Group分组，使用每组的列平均值填补该列的缺失值
  group_by(Group) %>%
  mutate(across(.cols = all_of(cols_to_fill), .fns = ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()
write_xlsx(data_filled, path = "C:\\Users\\ASUS\\Desktop\\SCZ_tACS\\ROI\\GrangerCausality\\GCvalue\\MANOVA_GCA_Filled.xlsx")
