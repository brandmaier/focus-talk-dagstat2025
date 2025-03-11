library(partykit)

set.seed(2309538)
N <- 10000
pred_mean <- sample(c(0,1),N,TRUE)
pred_var <- sample(c(0,1),N,TRUE)

y <- rnorm(N, mean=1*pred_mean, sd=1+pred_var)

simdata <- data.frame(y,pred_mean=as.factor(pred_mean),pred_var=as.factor(pred_var))
simdata <- data.frame(y,pred_mean, pred_var)


tree_partykit <- partykit::ctree(y~., simdata)

library(ggplot2)

ggplot(simdata, aes(x=y))+geom_histogram()+facet_wrap(~pred_mean+pred_var)


ggsave("img/simulated_four_univ.png",plot = last_plot())

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

cf <- partykit::cforest(y~., simdata)
vim_partykit <- partykit::varimp(cf)

sf <- semforest(sem, simdata, control = semforest_score_control(num.trees=100))
vim_semtree <- semtree::varimp(sf)

sf_f1 <- semforest(sem, simdata, control = semforest_score_control(num.trees=100),
                constraints=semtree.constraints(focus.parameters=c("mean_y")))
vim_semtree_f1 <- semtree::varimp(sf_f1,method = "permutationFocus")

sf_f2 <- semforest(sem, simdata, control = semforest_score_control(num.trees=100),
                   constraints=semtree.constraints(focus.parameters=c("var_y")))
vim_semtree_f2 <- semtree::varimp(sf_f2,method = "permutationFocus")


saveRDS( tree_semtree_f1, file="data/univsim_semtree_f1.rds" )
saveRDS( tree_semtree_f2, file="data/univsim_semtree_f2.rds" )
saveRDS( tree_semtree_f3, file="data/univsim_semtree_f3.rds" )
saveRDS(vim_partykit, file="data/univsim_vim_partykit.rds" )
saveRDS( vim_semtree_f1, file="data/univsim_vim_semtree_f1.rds" )
saveRDS( vim_semtree_f2, file="data/univsim_vim_semtree_f2.rds" )