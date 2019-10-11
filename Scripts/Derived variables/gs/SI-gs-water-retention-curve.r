
options(digits=22)
source("Scripts/Derived variables/SI-F.r")

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
h3 <- 25

# Environmental conditions
ca <- 400

# ESS
wL <- uniroot(ESSBf, c(0.01, 1), tol=.Machine$double.eps)$root
w <- seq(wL, 1, by=(1-wL)/100)
x <- psf(w)
y <- ESSf(w)

# Save file
resL <- data.frame(ps=x, gs=y)
colnames(resL) <- c("ps", "gs")
write.csv(resL, "Results/SI-gs-water-retention-curve-1.csv", row.names=FALSE)
