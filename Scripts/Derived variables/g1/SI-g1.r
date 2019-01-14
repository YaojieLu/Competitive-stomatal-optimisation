
options(digits=22)
source("Scripts/Derived variables/SI-F.r")

# Parameterization
LAI <- 1
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
# d
d <- 5

# Optimal g1
# Low h3
h3 <- 1

wL <- uniroot(ESSBf, c(0.01, 1), tol=.Machine$double.eps)$root
pxL <- psf(wL)
xg1 <- seq(pxL, psf(1), by=(psf(1)-pxL)/100)
yg1 <- ESSg1psf(xg1)

# Save file
resg1 <- data.frame(ps=xg1, g1=yg1)
colnames(resg1) <- c("ps", "g1")
write.csv(resg1, "Results/SI-g1L.csv", row.names = FALSE)

# High h3
h3 <- 100

wL <- uniroot(ESSBf, c(0.01, 1), tol=.Machine$double.eps)$root
pxL <- psf(wL)
xg1 <- seq(pxL, psf(1), by=(psf(1)-pxL)/100)
yg1 <- ESSg1psf(xg1)

# Save file
resg1 <- data.frame(ps=xg1, g1=yg1)
colnames(resg1) <- c("ps", "g1")
write.csv(resg1, "Results/SI-g1H.csv", row.names = FALSE)
