
source("Scripts/Figures/Figure-F.r")

# Parameterization
pe <- -1.58*10^-3
b <- 4.38
c <- 2.64
d <- 3.54
psL <- -4
pkx <- 0.6

# Define functions
PLCf1 <- function(px)PLCf(px)*100
PLCmax <- PLCf1(psL)
PLCmf <- function(px)PLCf1(x)-(PLCf1(x)-PLCf1(px))*pkx

# Figure
Cols <- c("purple", "orange", "darkgreen")
windows(24, 6)
par(mgp = c(2.2, 1, 0), xaxs = "i", lwd = 2, mar = c(0.5, 0.5, 0.5, 0.5), mfrow = c(1, 3), oma = c(3, 4, 0.5, 0.5))
# Left
x <- pe
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty = 2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[2])
curve(PLCmf, x, pe, col=Cols[3], add=T)

axis(1, pos=-4, lwd=2, at=c(0), cex.axis = 2)
axis(2, pos=-10, lwd=2, at=c(0, 50, 100), cex.axis = 2)
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.5)
mtext("PLC (%)", side=2, line=2.6, cex=1.5)
legend("bottomleft", c("Scenario I", "Scenario II", "Scenario III"), lty=c(1, 1, 1), col=Cols, cex = 2)
box()

# Middle
x <- -2
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty = 2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
curve(PLCmf, x, pe, col=Cols[2], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[3])
axis(1, pos=-4, lwd=2, at=c(0), cex.axis = 2)
axis(2, pos=-10, lwd=2, at=c(0, 50, 100), labels = FALSE)
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.5)

arrows(-0.7, PLCmf(-0.7)*0.8, -0.7, PLCf1(-0.7), length = 0.1, lty = 2, lwd = 1, col=Cols[2])
arrows(-0.7, PLCmf(-0.7)*1.2, -0.7, PLCf1(x), length = 0.1, lty = 2, lwd = 1, col=Cols[2])
arrows(x, PLCf1(x), -10, PLCf1(x), lty = 2, lwd = 1, length = 0.1)
arrows(x, PLCf1(x), x, -4, lty = 2, lwd = 1, length = 0.1)
text(x-0.8, 3, expression(psi[xL]), cex=2)
text(-8.3, PLCf1(x)-6, expression(PLC(psi[xL])), cex=2)
box()

# Right
x <- -4
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty = 2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
curve(PLCmf, x, pe, col=Cols[2], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[3])

axis(1, pos=-4, lwd=2, at=c(0), cex.axis = 2)
axis(2, pos=-10, lwd=2, at=c(0, 50, 100), labels = FALSE)
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.5)

arrows(-1, PLCmf(-1)*1.1, -1, PLCmax, lwd = 1, lty = 2, col=Cols[2])
arrows(-1, PLCmf(-1)*0.9, -1, PLCf1(-1), lwd = 1, lty = 2, col=Cols[2])
arrows(psL, PLCmax, psL, -4, lty = 2, lwd = 1, length = 0.1)
arrows(psL, PLCmax, -10, PLCmax, lty = 2, lwd = 1, length = 0.1)
text(x-1, 3, expression(psi[xL*infinity]), cex=2)
text(-8.6, PLCmax-6, expression(PLC[max]), cex=2)
box()

# Save file
dev.copy2pdf(file = "Figures/Figure 1.pdf")
