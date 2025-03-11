library(OpenMx)
library(semtree)

set.seed(123)
N <- 200

with_focus <- FALSE

social_network <- sample(x = c("sparse","dense"), size=N, replace=TRUE)
age <- sample(x = c("young","old"), size=N, replace=TRUE)
noise1 <- as.factor(sample(x = c(0,1),size=N, replace=TRUE))




Sigma <- matrix(byrow=TRUE,
                nrow=2,c(1,0.5,
                         0.5,1))
obs <- MASS::mvrnorm(N,mu=c(0,0),
                     Sigma=Sigma)
obs[,1] <- obs[,1] + ifelse(social_network=="dense",5,0)
obs[,2] <- obs[,2] + ifelse(age=="young",5,0)
df.biv <- data.frame(obs, social_network, age, noise1=noise1)
df.biv$social_network <- as.factor(social_network)
df.biv$age <- as.factor(age)
names(df.biv)[1:2] <- paste0("x",1:2)

#df.biv.pred <- data.frame(df.biv, 
#                          leaf=factor(df.biv$age*2+df.biv$social_network))

names(df.biv)[1:2] <- paste0("x",1:2)
manifests<-c("x1","x2")
model.biv <- mxModel("Bivariate_Model", 
                     type="RAM",
                     manifestVars = manifests,
                     latentVars = c(),
                     mxPath(from="x1",to=c("x1","x2"), 
                            free=c(TRUE,TRUE), value=c(1.0,.2) , 
                            arrows=2, label=c("VAR_happy","COV_happy_memory") ),
                     mxPath(from="x2",to=c("x2"), free=c(TRUE), 
                            value=c(1.0) , arrows=2, label=c("VAR_memory") ),
                     mxPath(from="one",to=c("x1","x2"), label=c("MEAN_happy","MEAN_memory"),
                            free=TRUE, value=0, arrows=1),
                     mxData(df.biv, type = "raw")
);
result <- mxRun(model.biv)



if (with_focus) cnst <- list(focus.parameters=fp) else cnst <- NULL

fp <- "MEAN_memory" # predicted by age
#fp <- "mu1" # predicted by social_network

forest <- semforest(model.biv, data=df.biv,
                    constraints = cnst,
                    control=semforest.control(num.trees=200, 
                                              control=semtree.control(method="score",alpha=1)))
saveRDS(forest, file=paste0("data/sim01_forest_focus",with_focus,".rds"))


if (with_focus) {
 vim <- varimp(forest, method="permutationFocus")
 plot(vim)
 saveRDS(vim, file=paste0("data/sim01_vim_focus",with_focus,".rds"))
}

vim2 <- varimp(forest)
plot(vim2)
saveRDS(vim2, file=paste0("data/sim01_vim2_focus",with_focus,".rds"))
