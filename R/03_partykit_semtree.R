
ntree <- 100
N <- 500

library(semtree)

set.seed(123)

x1 <- rbinom(N, size = 1, prob=.5)
x2 <- rbinom(N, size = 1, prob=.5)
x3 <- rbinom(N, size = 1, prob=.5)
y <- rnorm(N, x1+x2, sd=(x3/2+1))

x1 <- as.factor(x1)
x2 <- as.factor(x2)
x3 <- as.factor(x3)
sim.data <- data.frame(x1,x2,x3,y)


manifests <- "y"
observed.model <- mxModel("Univariate Normal Distribution",
                          type="RAM",
                          mxData(
                            sim.data,
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
                            labels=c("varx")
                          ),
                          
                          # means
                          mxPath(
                            from="one",
                            to=manifests,
                            arrows=1,
                            free=TRUE,
                            values=c(0),
                            labels=c("mux")
                          )
) # close model

observed.model <- mxRun(observed.model)

tree <- semtree(model = observed.model, data=sim.data)

future::plan("multicore",workers=7)

#forest <- semforest(model = observed.model, 
#                    data=sim.data, semforest_score_control(num.trees=100))
#vim<-varimp(forest)
#plot(vim)

sfc <- semforest_score_control()
sfc$num.trees <- ntree

forest2 <- semforest(model = observed.model, data=sim.data,
                     constraints=list(focus.parameters="mux"),
                     control=sfc)
vim2<-semtree::varimp(forest2, method="permutationFocus")



library(partykit)
cfor <- cforest(y~., data=sim.data,ntree=ntree)
vimc <- partykit::varimp(cfor)

par(mfrow=c(1,2))
barplot(sort(vimc,decreasing = TRUE),horiz = TRUE)
plot(vim2,sort=FALSE)
#plot(vim,sort=FALSE)

saveRDS(forest2, file="data/03_forest.rds")
saveRDS(vim2, file="data/03_vim2.rds")
saveRDS(vimc, file="data/03_vimc.rds")