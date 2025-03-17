#
# factor analysis of the bfi dataset
#
library(semtree)
library("psych")
data("bfi")



idx <- 1:5 # agreeableness
initial_loadings <- c(-1,+1,+1,+1,+1,+1)
#idx <- 6:10 # consc
#initial_loadings <- c(1,1,1,-1,-1)
idx <- 11:15 # extra
initial_loadings <- c(-1,-1,1,1,1)


preds <- c("education","age","gender")

bfisub <- bfi[,c(preds,names(bfi)[idx])]

bfisub <- bfisub[complete.cases(bfisub),]

# normalize ? 
for (nn in names(bfi)[idx]) {
  bfisub[nn] <- scale(bfisub[nn] ) 
}

bfisub$education <- ordered(bfisub$education, levels=c(1:5), labels=c("HS","finished HS","some college","college graduate","graduate degree"))
bfisub$gender <- factor(bfisub$gender,levels=c(1,2), labels=c("male","female"))

manifests <- names(bfi)[idx] 
latents <- c("G")
factorModel <- mxModel("One Factor", type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       
                       mxPath(from=latents, to=manifests, labels= paste("f",1:5,sep=""),
                              free=c(F,T,T,T,T),
                              values=initial_loadings
                              
                       ),
                       mxPath(from=manifests, arrows=2, labels= paste("e",1:5,sep="")     
                       ),
                       mxPath(from=latents, arrows=2,
                              free=T, values=1.0, label=c("lat_var")),     
                       mxPath(from="one", to=manifests, values=0.0,free=c(F,T,T,T,T),labels=paste("meanF",1:5,sep="")),
                       mxPath(from="one", to=latents, values=1.0,free=T,labels="meanG"),
                       mxData( bfisub, type="raw" )
);

factorModel2 <- mxModel("One Factor", type="RAM",
                        manifestVars = manifests,
                        latentVars = latents,
                        
                        mxPath(from=latents, to=manifests, labels= paste("f",1:5,sep=""),
                               free=c(T,T,T,T,T),
                               values=initial_loadings
                               
                        ),
                        mxPath(from=manifests, arrows=2, labels= paste("e",1:5,sep="")     
                        ),
                        mxPath(from=latents, arrows=2,
                               free=F, values=1.0, label=c("lat_var")),     
                        mxPath(from="one", to=manifests, values=0.0,free=c(T,T,T,T,T),labels=paste("meanF",1:5,sep="")),
                        mxPath(from="one", to=latents, values=0.0,free=F,labels="meanG"),
                        mxData( bfisub, type="raw" )
);

run <- mxRun(factorModel)
run2 <- mxRun(factorModel2)
#options <- semtree.control()

tree <- semtree(model=run2, bfisub, 
                control = semtree.control(method="score"),
                constraints=list(focus.parameters=c("f2","f3","f4","f5")))

plot(tree)

tree3<-plot(prune(tree,3),)

leafs<-semtree::getLeafs(prune(tree,3))
params<-sapply(leafs, function(x) {omxGetParameters(x$model)[paste0("f",2:5)]})
#age <- c(12.5,26,11,25,32,45,65,65)
#edu <- c(0,0,1,1,0.5,0.5,0,1)


plot(prune(tree,2))

plot(tree)

tt <- data.frame(toTable(tree)[,1:8])
tt[,"f1"] <- as.numeric(tt[,"f1"])
tt[,"f2"] <- as.numeric(tt[,"f2"])
tt[,"f3"] <- as.numeric(tt[,"f3"])
tt[,"f4"] <- as.numeric(tt[,"f4"])
tt[,"f5"] <- as.numeric(tt[,"f5"])
tt$label <- paste0(tt$gender," AND ",tt$age, " AND ",tt$education)
tt <- tt[, -c(1:3)]
tt$label <- c(" female age<23", " female age>=23, no college",
              "female age>=23, c. graduate", "Male")
library(dplyr)
library(ggplot2)

tidyr::pivot_longer(tt, 1:5) %>% ggplot(aes(y=value,x=name, fill=name))+geom_col()+facet_wrap(~label,ncol=4)

ggsave(filename="img/06bfi-tree-loadings.png", plot=last_plot(), width = 6,height=3)

frst <- semforest(model=run2, bfisub, 
                control = semforest_score_control(num.trees=100),
                constraints=list(focus.parameters=c("f2","f3","f4","f5")))

vim <- varimp(frst, method="permutationFocus")

plot(vim)

saveRDS(object=vim, file="data/06_bfi_vim.rds")
