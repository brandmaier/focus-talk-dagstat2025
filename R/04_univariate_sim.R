library(partykit)
library(tictoc)
library(future)

plan(multisession, workers = 7)
ntrees <- 100

set.seed(2309538)
N <- 1000
pred_mean <- sample(c(0,1),N,TRUE)
pred_var <- sample(c(0,1),N,TRUE)

noise1 <- sample(c(0,1),N,TRUE)
noise2 <- sample(c(0,1),N,TRUE)
noise3 <- sample(c(0,1),N,TRUE)

y <- rnorm(N, mean=1*pred_mean, sd=1+pred_var)

simdata <- data.frame(y,
                      pred_mean=as.factor(pred_mean),
                      pred_var=as.factor(pred_var),
                      noise1=as.factor(noise1),
                      noise2=as.factor(noise2),
                      noise3=as.factor(noise3)
                      )
#simdata <- data.frame(y,pred_mean, pred_var, noise1, noise2, noise3)


tree_partykit <- partykit::ctree(y~., simdata)

saveRDS(tree_partykit, file="data/04_univsim_tree_partykit.rds")

library(ggplot2)

ggplot(simdata, aes(x=y))+geom_histogram()+facet_wrap(~pred_mean+pred_var)



#plot(tree)

library(semtree)

library(OpenMx)
manifests <- "y"
sem <- mxModel("Univariate Normal Distribution",
                          type="RAM",
                          mxData(
                            simdata,
                            type="raw"
                          ),
                          manifestVars=manifests,
                          latentVars=c(),
                          # variance
                          mxPath(
                            from=manifests,
                            arrows=2,
                            free=TRUE,
                            values = c(1),
                            labels=c("var_y")
                          ),
                          
                          # means
                          mxPath(
                            from="one",
                            to=manifests,
                            arrows=1,
                            free=TRUE,
                            values=c(0),
                            labels=c("mean_y")
                          )
) # close model

tree_semtree <- semtree(sem, simdata, control=semtree.control(method="score"))

tree_semtree_f1 <- semtree(sem, simdata, control=semtree.control(method="score"),
                              constraints = semtree.constraints(focus.parameters=c("mean_y")))

tree_semtree_f2 <- semtree(sem, simdata, control=semtree.control(method="score"),
                           constraints = semtree.constraints(focus.parameters=c("var_y")))

#tree_semtree_f3 <- semtree(sem, simdata, control=semtree.control(method="naive"),
#                           constraints = semtree.constraints(focus.parameters=c("mean_y")))

plot(tree_semtree_f1)
plot(tree_semtree_f2)

tic()
cf <- partykit::cforest(y~., simdata)
vim_partykit <- partykit::varimp(cf)
#vim_partykit2 <- partykit::varimp(cf,risk=c("misclassification"))
toc()

tic()
sf <- semforest(sem, simdata, control = semforest_score_control(num.trees=ntrees))
vim_semtree <- semtree::varimp(sf)
toc()

tic()
sf_f1 <- semforest(sem, simdata, control = semforest_score_control(num.trees=ntrees),
                constraints=semtree.constraints(focus.parameters=c("mean_y")))
vim_semtree_f1 <- semtree::varimp(sf_f1,method = "permutationFocus")
vim_semtree_f1old <- semtree::varimp(sf_f1)
toc()

tic()
sf_f2 <- semforest(sem, simdata, control = semforest_score_control(num.trees=ntrees),
                   constraints=semtree.constraints(focus.parameters=c("var_y")))
vim_semtree_f2 <- semtree::varimp(sf_f2,method = "permutationFocus")
toc()

saveRDS( tree_semtree, file="data/04_univsim_semtree.rds" )
saveRDS( tree_semtree_f1, file="data/04_univsim_semtree_f1.rds" )
saveRDS( tree_semtree_f2, file="data/04_univsim_semtree_f2.rds" )
#saveRDS( tree_semtree_f3, file="data/04_univsim_semtree_f3.rds" )
saveRDS(vim_partykit, file="data/04_univsim_vim_partykit.rds" )
saveRDS( vim_semtree, file="data/04_univsim_vim_semtree.rds" )
saveRDS( vim_semtree_f1, file="data/04_univsim_vim_semtree_f1.rds" )
saveRDS( vim_semtree_f1old, file="data/04_univsim_vim_semtree_f1old.rds" )
saveRDS( vim_semtree_f2, file="data/04_univsim_vim_semtree_f2.rds" )
saveRDS ( sf, file="data/04_sf.rds")
saveRDS ( sf_f1, file="data/04_sf_f1.rds")
saveRDS ( sf_f2, file="data/04_sf_f2.rds")

ggsave("img/04_simulated_four_univ.png",plot = last_plot())
