
# Data
data1   <- read.csv("Results/SIII-gs.csv")
data2   <- read.csv("Results/SIII-gs-water-retention-curve-2.csv")

# Figures
windows(16, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.3, 3.5, 2, 0.7), mfrow=c(1, 2))
# gs - w
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.3), cex.lab=1.3, main = expression(ESS~italic(g[s]*(s))))
points(data1, type="l")
points(data2, type="l", col=2)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=2, cex=1.3)
axis(2, ylim=c(0, 0.3), pos=0, lwd=2, at = c(0, 0.1, 0.2, 0.3))
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)

legend("bottomright", c("Campbell model", "van Genuchten model"), lty=1, col=c(1, 2))
box()
# Soil water retention curve
psf1 <- function(w, pe = -1.58*10^-3, b = 4.38)pe*w^(-b)
psf2 <- Vectorize(function(s, psa = 0.209, psb = 1.236, psc = 0.258){
  f1 <- function(ps)s - 1/((1+(-ps/psa)^psb)^psc)
  res <- uniroot(f1, c(-1000000, 0), tol=.Machine$double.eps)$root
  return(res)
})
curve(psf1, 0.15, 1, xaxt="n", yaxt="n", xlab=NA, ylab=NA,
      xlim = c(0, 1), ylim = c(-10, 0), cex.lab=1.3, main = "Soil water retention curve")
curve(psf2, 0.35, 1, add = T, col = 2)

axis(1, xlim=c(0, 1), pos = -10, lwd=2)
mtext(expression(italic(s)),side=1, line=2, cex=1.3)
axis(2, ylim=c(-10, 0), pos=0, lwd=2)
mtext(expression(italic(psi[s])~(MPa)),side=2,line=1.8, cex=1.3)

