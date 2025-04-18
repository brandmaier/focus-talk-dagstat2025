#
# This model specification was automatically generated by Onyx
# 
library(semtree)
require("OpenMx")
library(semper)
library(tictoc)

manifests<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9")

N <-100
modelData <- data.frame(matrix(rnorm(N*9),ncol=9))
names(modelData) <- manifests



result <- mxRun(model)
summary(result)

getModel <- function(e=1, slope_var=1,
                     icept_var=1, icept_slope_cov=0) {
  model<-semper::lgcm(timepoints = c(0:4),intercept.variance = icept_var,slope.variance = slope_var,
              intercept.slope.covariance = icept_slope_cov, residual.variance = e)
 
  return(semper::toOpenMx(model))
}

generate <- function(Ng = 200) {

dat1 <- mxGenerateData(getModel(slope_var=2, icept_var=2,e=1),nrows = Ng)
dat2 <- mxGenerateData(getModel(slope_var=5, icept_var=2,e=1),nrows = Ng)
dat3 <- mxGenerateData(getModel(slope_var=2,icept_var=5,e=1),nrows = Ng)
dat4 <- mxGenerateData(getModel(slope_var=5,icept_var=5,e=1),nrows = Ng)


pred_slope <- as.ordered(rep(c(0,1,0,1,0,1,0,1),each=Ng))
pred_icept <- as.ordered(rep(c(0,0,1,1,0,0,1,1),each=Ng))
noise <- as.ordered(rep(c(0,0,0,0,1,1,1,1),each=Ng))
df<-data.frame(rbind(dat1,dat2,dat3,dat4),pred_slope,pred_icept,pred3)
return(df)
}

df <- generate(200)

model <- getModel()
model@data <- OpenMx::mxData(observed=df, type = "raw")
model <- mxRun(model)

tree_icept<-semtree(model, data=df, control = semtree.control(method="score"),
               constraints=semtree.constraints(focus.parameters = "interceptvariance"))

tree_slope<-semtree(model, data=df, control = semtree.control(method="score"),
               constraints=semtree.constraints(focus.parameters = "slopevariance"))

plot(tree_icept)
plot(tree_slope)


library(future)
plan(multisession, workers = 7)
tic()
sf_icept <- semforest(model, data=df, 
                      control=semforest_control(num.trees=100),
                      constraints=semtree.constraints(focus.parameters = "interceptvariance"))
toc()
tic()
vim_icept <- semtree::varimp(sf_icept,method = "permutationFocus")
toc()

tic()
sf_slope <- semforest(model, data=df, 
                      control=semforest_control(num.trees=100),
                      constraints=semtree.constraints(focus.parameters = "slopevariance"))
toc()
tic()
vim_slope <- semtree::varimp(sf_slope,method = "permutationFocus")
toc()


tic()
sf_err <- semforest(model, data=df, 
                      control=semforest_control(num.trees=100),
                      constraints=semtree.constraints(focus.parameters = "residualvariance"))
toc()
tic()
vim_err <- semtree::varimp(sf_err,method = "permutationFocus")
toc()

saveRDS(object = vim_err, file="data/08_lgcm_vim_err.rds")
saveRDS(object = vim_slope, file="data/08_lgcm_vim_slope.rds")
saveRDS(object = vim_icept, file="data/08_lgcm_vim_icept.rds")


par(mfrow=c(1,3),mar=c(0.1))
plot(icept,xlim=c(0,50))
plot(err,xlim=c(0,50))
plot(slope, xlim=c(0,50))

library(tidyverse)
library(dplyr)

set.seed(234)
df <- generate(50)
df$id <- 1:nrow(df)
df %>% pivot_longer(1:5) %>% ggplot(aes(x=name,y=value,
                                        group=id,
                                        color=interaction(pred_slope,pred_icept)))+geom_line(alpha=1)+
                                   #)+
                                  #   geom_line()+
  facet_wrap(~pred_slope+pred_icept)+theme(legend.position = "none") 

ggsave(filename="img/lgcm_simu_4.png",plot = last_plot())
