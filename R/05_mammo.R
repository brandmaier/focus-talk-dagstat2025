data("mammoexp", package = "TH.data")
mtree <- ctree(ME ~ ., data = mammoexp)

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

tree<-semtree(sem, data=mammoexp, control=semtree.control(min.bucket = 50),
              constraints = semtree.constraints(focus.parameters=c("mean_y")))

plot(tree)

saveRDS(object=tree, file="dat/mammo_semtree.rds")

params <- OpenMx::omxGetParameters(tree$left_child$model)

area_inf_neg1<-pnorm( -1, mean=params[2],sd=sqrt(params[1]))
area_inf_pos1<-pnorm( +1, mean=params[2],sd=sqrt(params[1]))
area_pos1_inf<-1-pnorm( +1, mean=params[2],sd=sqrt(params[1]))
vals <- c(area_inf_neg1, area_inf_pos1-area_inf_neg1, area_pos1_inf )


mean_value <- params[2]
variance_value <- params[1]
sd_value <- sqrt(variance_value)

# Generate x values
x_vals <- seq(mean_value - 4 * sd_value, mean_value + 4 * sd_value, length.out = 300)

# Compute normal density
density_vals <- dnorm(x_vals, mean = mean_value, sd = sd_value)
density_vals <- pnorm(x_vals, mean = mean_value, sd = sd_value)

# Create dataframe
data <- data.frame(x = x_vals, y = density_vals)

# Plot
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +  # Normal distribution curve
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "red", size = 1) + # Thresholds
  labs(title = "Thresholds",
       x = "X", y = "Density") +
  theme_minimal()

ggsave(filename="img/plot_mammo_partytree.png")

#saveRDS(object=, file="dat/")