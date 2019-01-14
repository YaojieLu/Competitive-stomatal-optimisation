
options(digits=22)
source("Scripts/Derived variables/SII-F.r")
data <- read.csv("Results/SII-DV.csv")

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
#d <- 3.54
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

# Environmental conditions
ca <- 400
k <- 0.05
MAP <- 1825
gamma <- 1/((MAP/365/k)/1000)*nZ
pkx <- 0.5
d <- 5

# ESS gs
# Low h3
h3L <- 1

wLL <- subset(data, h3==h3L & d==5, select="wL")[[1]]
h3 <- h3L
wLLL <- wLLf(wLL)
fL <- Vectorize(function(w)gswLf(w, wLL))
xL <- seq(wLLL, 1, by=(1-wLLL)/100)
yL <- fL(xL)

# Save file
resL <- data.frame(w=xL, gs=yL)
colnames(resL) <- c("w", "gs")
write.csv(resL, "Results/SII-gsL.csv", row.names=FALSE)

# High h3
h3H <- 100

wLH <- subset(data, h3==h3H & d==5, select="wL")[[1]]
h3 <- h3H
wLHL <- wLLf(wLH)
fH <- Vectorize(function(w)gswLf(w, wLH))
xH <- seq(wLHL, 1, by=(1-wLHL)/100)
yH <- fH(xH)

# Save file
resH <- data.frame(w=xH, gs=yH)
colnames(resH) <- c("w", "gs")
write.csv(resH, "Results/SII-gsH.csv", row.names=FALSE)
