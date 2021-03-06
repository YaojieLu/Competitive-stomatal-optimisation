
source("Scripts/Figures/Figure-F.r")

# Parameterization
LAI <- 3
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

# Data
dataSIL   <- read.csv("Results/SI-gsL.csv")
dataSIH   <- read.csv("Results/SI-gsH.csv")
dataSIIL  <- read.csv("Results/SII-gsL.csv")
dataSIIH  <- read.csv("Results/SII-gsH.csv")
dataSIII <- read.csv("Results/SIII-gs.csv")

# Figures
Cols <- c("purple", "orange", "darkgreen")
windows(24, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 4.6, 0.6, 0.7), mfrow=c(1, 3))
# gs - w
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.4), cex.lab=1.3)
points(dataSIL, type="l", col=Cols[1])
points(dataSIH, type="l", col=Cols[1], lty=2)
points(dataSIIL, type="l", col=Cols[2])
points(dataSIIH, type="l", col=Cols[2], lty=2)
points(dataSIII, type="l", col=Cols[3])

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=2, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=2.2, cex=1.3)
text(1*0.04, 0.4*0.945, expression((bold(a))), cex=1.3)
box()

# gs - ps
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(0, 0.4), cex.lab=1.3)
points(psf(dataSIL[[1]]), dataSIL[[2]], type="l", col=Cols[1])
points(psf(dataSIH[[1]]), dataSIH[[2]], type="l", col=Cols[1], lty=2)
points(psf(dataSIIL[[1]]), dataSIIL[[2]], type="l", col=Cols[2])
points(psf(dataSIIH[[1]]), dataSIIH[[2]], type="l", col=Cols[2], lty=2)
points(psf(dataSIII[[1]]), dataSIII[[2]], type="l", col=Cols[3])

axis(1, xlim=c(-10, 0), pos=0, lwd=2)
mtext(expression(italic(psi[s])~(MPa)),side=1, line=3, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=-10, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)
text(-10*0.04, 0.4*0.945, expression((bold(b))), cex=1.3)
legend("topleft", c(expression(paste(italic(p[k]==100), ",  ", beta==1)),
                    expression(paste(italic(p[k]==100), ",  ", beta==100)),
                    expression(paste(italic(p[k]==50), ",  ", beta==1)),
                    expression(paste(italic(p[k]==50), ",  ", beta==100)),
                    expression(paste(italic(p[k]==0)))), lty=c(1, 2, 1, 2, 1), col=c("purple", "purple", "orange", "orange", "darkgreen"))
box()

# rainfall
source("Scripts/Figures/Figure 3c.r")

# Save file
dev.copy2pdf(file = "Figures/Figure 3.pdf")
