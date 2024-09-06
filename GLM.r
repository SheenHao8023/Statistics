# 加载必要的R包
library(readxl)
library(dplyr)
library(car)
library(purrr)
data <- read_excel('C:/Users/ASUS//Desktop/AI_SCZ/Analysis_combine/GLM.xlsx')
glm_data <- data %>%
            select(TS, everything()[4:ncol(data)])
glm_model <- glm(TS ~ ., data = glm_data, family = gaussian())
# vifs <- vif(glm_model)
# print(vifs)
# 或者，如果你只想知道是否存在VIF大于特定阈值的情况
# high_vif_vars <- names(vifs[vifs > 10]) # 将10改为你的阈值
# print(high_vif_vars)


check_and_remove_vif <- function(model, threshold = 10) {
  vifs <- vif(model)
  # 找出VIF超过阈值的变量
  high_vifs <- names(vifs[vifs > threshold])
  if (length(high_vifs) > 0) {
    cat("Removing variables with high VIF:\n")
    print(high_vifs)
    # 从模型中移除具有最高VIF的变量
    model <- update(model, . ~ . - first(high_vifs))
    # 递归调用自身，直到所有VIF都低于阈值
    return(check_and_remove_vif(model, threshold))
  } else {
    return(model)
  }
}

# 使用定义的函数检查并移除高VIF变量
final_model <- check_and_remove_vif(glm_model)

# 再次计算VIF以确认问题已被解决
vifs_final <- vif(final_model)
print(vifs_final)


summary(glm_model)