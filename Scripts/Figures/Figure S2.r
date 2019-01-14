
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

# ESS derivation
h3 <- 25
wLL <- subset(data, h3==25 & d==5, select="wL")[[1]]
wLLL <- wLLf(wLL)
fL <- Vectorize(function(w)gswLf(w, wLL))
xL <- seq(wLLL, 1, by=(1-wLLL)/100)
yL <- fL(xL)
res <- data.frame(w=xL, E=yL*VPD*h)
colnames(res) <- c("w", "E")

# Figure
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.3, 3.5, 0.9, 0.7), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.04), cex.lab=1.3)
points(res, type = "l")
f <- function(x)x/100
curve(f, 0, 1, lty = 2, add = T)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=2, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(E~or~DI~(day^-1))),side=2,line=1.8, cex=1.3)
legend("topleft", c(expression(italic(E)), expression(italic(DI))), lty=c(1, 2))
box()

# Save file
dev.copy2pdf(file = "Figures/Figure S2.pdf")
