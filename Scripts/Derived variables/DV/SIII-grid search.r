
options(digits=22)
source("Scripts/Derived variables/SIII-F.r")

# Parameterization
LAI <- 3
Vcmax <- 50
cp <- 30
Km <- 703
Rd <- 1
a <- 1.6
nZ <- 0.5
p <- 43200
l <- 1.8e-5
VPD <- 0.02
pe <- -1.58*10^-3
b <- 4.38
kxmax <- 5
c <- 2.64
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

# Environmental conditions
ca <- 400
k <- 0.05
MAP <- 1825
gamma <- 1/((MAP/365/k)/1000)*nZ
d <- 5

# Initialize
ESS <- psf(0.152775311364286)
x_p <- seq(ESS-5, ESS+5, by = 0.5)
x_w <- wf(x_p)
x <- as.vector(expand.grid(x_w, x_w))
y <- data.frame("R" = x[, 1], "I" = x[, 2], "B_bar" = x[, 2])

# Run
for(i in 1:nrow(y)){
  y[i, 3] = averAirelf(y[i, 2], y[i, 1])
  print(nrow(y) - i)
}
y[, 1:2] <- psf(y[, 1:2])

# Save file
write.csv(y, "Results/SIII-grid_search.csv", row.names = FALSE)
