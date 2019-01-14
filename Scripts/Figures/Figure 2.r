
library(plotly)

# Data
data   <- read.csv("Results/SIII-grid_search.csv")
x <- unique(data$R)
y1 <- unique(data$R)
n <- length(x)
z1 <- t(matrix(data[, 3],nrow = n, ncol = n))
diag1 <- diag(z1)
z2 <- matrix(diag1, nrow = 1, ncol = n)
z2 <- z2[rep(1:nrow(z2), times = n), ]
z3 <- z2 - z1
y2 <- y1[apply(z1, 2, which.max)]
z4 <- c(ifelse(z3 < 0, 0, 1))
plane <- matrix(rep(-100, length(z3)), nrow = n, ncol = n)

# Figure
# Color
color <- z4
dim(color) <- dim(z3)

p <- plot_ly(colors = c('blue', 'red')) %>%
  layout(scene = list(xaxis = list(title = "A", range = c(-10, 0)),
                         yaxis = list(title = "B", range = c(-10, 0)),
                         zaxis = list(title = "C", range = c(-100, 50)))) %>%
  # 3D surface
  add_surface(x = x, y = y1, z = z3,
              opacity = 0.8, surfacecolor = color,
              cauto = F, cmax = 1, cmin = 0) %>%
  # Plane
  add_surface(x = x, y = y1, z = plane,
              opacity = 0.8, surfacecolor = color,
              cauto = F, cmax = 1, cmin = 0) %>%
  hide_colorbar()
# White lines
for(i in 1:n){
  p <- add_trace(p, x = rep(x[i], n), y = y1, z = z3[, i],
                 type = "scatter3d", mode = "lines", showlegend = FALSE,
                 line = list(color = "white", width = 4))
  p <- add_trace(p, x = rep(x[i], n), y = y1, z = plane[, i],
                 type = "scatter3d", mode = "lines", showlegend = FALSE,
                 line = list(color = "white", width = 4))
}
p
