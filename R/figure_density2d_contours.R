library(ggplot2)
library(MASS)  # For mvrnorm

grids <-  list()

for (i in 0:3) {

# Define the mean and covariance matrix
mu <- c( i %% 2,  i %/% 2) * 5
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)  # Covariance matrix

# Create a grid of (x, y) values
x_seq <- seq(-5, 8, length.out = 100)
y_seq <- seq(-5, 8, length.out = 100)
grid <- expand.grid(X = x_seq, Y = y_seq)

# Compute the 2D Gaussian density function
gaussian_density <- function(x, y, mu, sigma) {
  z <- c(x, y) - mu
  exp(-0.5 * t(z) %*% solve(sigma) %*% z) / (2 * pi * sqrt(det(sigma)))
}

# Apply the function to each grid point
grid$Z <- apply(grid, 1, function(row) gaussian_density(row[1], row[2], mu, sigma))

grids[[length(grids)+1]]<-grid

}

scheme <- 3

cols1 <- c("orange","black","blue","purple")
cols2 <- c("orange","black","orange","black")
cols3 <- c("orange","orange","black","black")
cols <- list(cols1, cols2, cols3)

cur_col <- cols[[scheme]]

br <- NULL
# Plot using ggplot2
ggplot(grids[[1]],aes(x = X, y = Y, z = Z)) +
  geom_contour(data=grids[[1]],color=cur_col[1],breaks =br,lwd=1) + 
  geom_contour(data=grids[[2]],color=cur_col[2],breaks =br,lwd=1) + 
  geom_contour(data=grids[[3]],color=cur_col[3],breaks =br,lwd=1) + 
  geom_contour(data=grids[[4]],color=cur_col[4],breaks =br,lwd=1) + 
  scale_color_viridis_c() + 
 # coord_cartesian(xlim = c(-3, 5), ylim = c(-3, 5))+
  theme_minimal() +
  labs(#title = "2D Gaussian Contour Plot",
       x = "Episodic Memory Performance",
       y = "Happiness")


ggsave(filename=paste0("img/contourplot2d-simulation-4colors-scheme",scheme,".png"),plot = last_plot(),width = 3,height=3)
