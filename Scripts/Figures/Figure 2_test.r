
# Data
data   <- read.csv("Results/SIII-grid_search_test.csv")

# Figure
Cols <- c("grey68", "grey8")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 4, 2.5, 0.5), mfrow=c(1, 1))
plot(0,
     xaxt="n", yaxt="n", xlab=NA, ylab=NA, type = "n",
     xlim = c(-10, 0), ylim = c(-10, 0), col = Cols[1])
rect(-10, -10, 0, 0, border = Cols[2], col = Cols[2])
points(data[, 1], data[, 3], col = Cols[1], type = "l")
abline(a = 0, b = 1, col = Cols[1])
polygon(c(0, -10, data[1:ceiling(nrow(data)/2), 1], 0), c(0, 0, data[1:ceiling(nrow(data)/2), 3], 0), border = NA, col = Cols[1])
polygon(c(0, -10, data[ceiling(nrow(data)/2):nrow(data), 1]), c(-10, -10, data[ceiling(nrow(data)/2):nrow(data), 3]), border = NA, col = Cols[1])

axis(1, xlim = c(-10, 0), pos = -10, lwd = 2, at = c(-10, -5, 0))
mtext(expression(psi[xL*infinity]~(MPa)~"for"~italic(R)), side = 1, line = 3, cex = 2)
axis(2, ylim = c(-10, 0), pos = -10, lwd = 2, at = c(-10, -5, 0))
mtext(expression(psi[xL*infinity]~(MPa)~"for"~italic(I)), side = 2, line = 1.8, cex = 2)
legend("top", c(expression(bar(B[R])>=bar(B[I])), expression(bar(B[R])<bar(B[I]))),
       pch = 15, col = c(Cols[1], Cols[2]),
       horiz = TRUE, bty = "n", xpd = TRUE, inset = c(0, -0.14), cex = 2)
box()

# Save file
dev.copy2pdf(file = "Figures/Figure 2.pdf")
