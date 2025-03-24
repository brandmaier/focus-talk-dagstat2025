library(tidyverse)
library(ggplot2)
library(party)
data("mammoexp", package = "TH.data")
mtree <- partykit::ctree(ME ~ ., data = mammoexp)

plot(mtree)

manifests <- "ME"

threshold <- mxThreshold(vars=manifests, nThresh=c(2), free = c(FALSE,FALSE),values=c(-1,+1))


library(OpenMx)

sem <- mxModel("Univariate Normal Distribution",
               type="RAM",
               mxData(
                 mammoexp,
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
               ),
               threshold
) # close model

fit <- mxRun(sem)

summary(fit)

#tree<-semtree(sem, data=mammoexp, control=semtree.control(min.bucket = 50,bonferroni = TRUE),
#              constraints = semtree.constraints(focus.parameters=c("mean_y")))

tree_nc<-semtree(sem, data=mammoexp, control=semtree.control(min.bucket = 50,bonferroni = TRUE))

plot(tree_nc)

saveRDS(tree, file="data/mammo_semtree.rds")

params1 <- OpenMx::omxGetParameters(tree$left_child$model)
params2 <- OpenMx::omxGetParameters(tree$right_child$left_child$model)
params3 <- OpenMx::omxGetParameters(tree$right_child$right_child$model)

area_inf_neg1<-pnorm( -1, mean=params[2],sd=sqrt(params[1]))
area_inf_pos1<-pnorm( +1, mean=params[2],sd=sqrt(params[1]))
area_pos1_inf<-1-pnorm( +1, mean=params[2],sd=sqrt(params[1]))
vals <- c(area_inf_neg1, area_inf_pos1-area_inf_neg1, area_pos1_inf )

ff <- function(params) {
mean_value <- params[2]
variance_value <- params[1]
sd_value <- sqrt(variance_value)

# Generate x values
x_vals <- seq(mean_value - 4 * sd_value, mean_value + 4 * sd_value, length.out = 300)

# Compute normal density
#density_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
density_vals <- pnorm(x_vals, mean = mean_value, sd = sd_value)

# Create dataframe
data <- data.frame(x = x_vals, y = density_vals)
}

data <- ff(params1)

# Plot
ggplot2::ggplot(data, aes(x = x, y = y)) +
  geom_line(data=ff(params1),color = "blue", linewidth = 2) +  # Normal distribution curve
  geom_line(data=ff(params2),color = "red", linewidth = 1) +  # Normal distribution curve
  geom_line(data=ff(params3),color = "green", linewidth = 1) +  # Normal distribution curve
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black", size = 1) + # Thresholds
  labs(title = "Thresholds",
       x = "X", y = "Density") +
  coord_cartesian(xlim=c(-15,10))+
  theme_minimal()

ggsave(filename="img/05_plot_mammo_partytree.png",plot=last_plot())


saveRDS(object=tree_nc, file = "data/05_mammo_tree.rds")
saveRDS(object=mtree, file="data/05_mammo_partytree.rds")
#saveRDS(object=, file="dat/")