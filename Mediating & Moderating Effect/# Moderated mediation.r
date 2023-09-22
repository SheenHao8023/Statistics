# Moderated mediation

install.packages('bruceR')  #install bruceR package

library(bruceR)  #load package
data = SourceData_0802
PROCESS(data, y='ZRes', x='ZRep',
        meds='ZBul',
        mods='ZMea',
        covs=c('ZGen','ZGra','ZAge','ZEsc','ZImm'),
        mod.path=c('m-y', 'x-y'),
        ci='mcmc', nsim=1000, seed=1)
PROCESS(data, y='ZRes', x='ZBul',
        mods='ZMea',
        covs=c('ZGen','ZGra','ZAge','ZEsc','ZImm'),
        ci='mcmc', nsim=1000, seed=1)->P
P$results[[1]]$jn[[1]]       # Johnson-Neyman interval
P$results[[1]]$jn[[1]]$plot  # Johnson-Neyman plot (ggplot object)
GLM_summary(P$model.y)       # detailed results of regression