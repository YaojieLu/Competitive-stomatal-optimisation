
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
d <- 5
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

# Environmental conditions
ca <- 400
k <- 0.05
MAP <- 1825
gamma <- 1/((MAP/365/k)/1000)*nZ

# ESS
wL <- uniroot(optwLf, c(0.01, 0.6), tol=.Machine$double.eps)$root#0.33913342975739968
fgs <- function(w)gswLf(w, wL)
xgs <- seq(wL, 1, by=(1-wL)/100)
ygs <- fgs(xgs)

# Save file
res <- data.frame(w=xgs, gs=ygs)
colnames(res) <- c("w", "gs")
write.csv(res, "Results/SIII-gs-water-retention-curve-2.csv", row.names=FALSE)
