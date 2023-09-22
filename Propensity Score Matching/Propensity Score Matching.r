#倾向得分匹配 Propensity Score Matching

install.packages("MatchIt")  #install MatchIt package
library(MatchIt)  #load MatchIt package

PSM = matchit(matching variable ~ covariate1 + covariate2 + covariate3, data=dataset, method="nearest", distance="logit", ratio=1, caliper=0.02, replace=FALSE)
#method:默认最临近匹配(method="nearest",nearest neighbor matching), 其他方法还有"exact" (exact matching), "full" (full matching), "optimal" (optimal matching), "subclass" (subclassification),"genetic" (genetic matching), and "cem" (coarsened exact matching)
#distance: 计算倾向值默认采用的是logistic回归(distance="logit"), 如想采用其他链接函数如Probit回归来计算倾向值, 可添加distance="probit"
#ratio: the number of control units to match to each treated unit (default = 1),设置匹配比例, 如果想干预组和对照组按1:n进行匹配, ratio=n即可
#caliper: the number of standard deviations of the distance measure within which to draw control units (default = 0, no caliper matching), 卡钳值
#replace: 默认replace=FALSE表示每个对照最多使用1次, replace=TRUE允许每个对照匹配多个干预
summary(PSM)  #Output

plot(PSM) #Q-Q graph
plot(PSM,type="hist",col=7)  #histogram
plot(PSM,type="jitter",col=6)  #jitter diagram of PS, test of common support: PS(Group1)min <= PS <= PSmax(Group2)

summary(PSM, standardize = T)$sum.matched  #SMD(standard mean difference), SMD<0.1, matching is acceptable

PSMed = match.data(PSM)
str(PSMed)  #check matched data
table(PSMed$matching variable)  #check the number of matched sample

tapply(PSMed$dependent variable, PSMed$matching variable, mean)  #check mean of dependent variable
t.test(PSMed$dependent variable ~ PSMed$matching variable)  #T-test, test of balancing, Average Treatment Effects on Treated(ATT Analysis)
Regmodel = with(PSMed, lm(dependent variable ~ matching variable))
summary(Regmodel)  #linear regression

