
source("Scripts/Derived variables/SIII-F.r")
data   <- read.csv("Results/SIII-DV-rainfall.csv")

gsf1 <- function(w)gswLf(w, data[1, 3])
gsf2 <- function(w)gswLf(w, data[2, 3])
gsf3 <- function(w)gswLf(w, data[3, 3])
gsf4 <- function(w)gswLf(w, data[4, 3])
gsf5 <- function(w)gswLf(w, data[5, 3])
gsf6 <- function(w)gswLf(w, data[6, 3])
gsf7 <- function(w)gswLf(w, data[7, 3])
gsf8 <- function(w)gswLf(w, data[8, 3])
gsf9 <- function(w)gswLf(w, data[9, 3])

# Figure
plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
     xlim = c(0, 1), ylim = c(0, 0.4), cex.lab = 1.3)
curve(gsf1, data[1, 3], 1, add = T, col = "darkgreen", lwd = 4, lty = 1)
curve(gsf2, data[2, 3], 1, add = T, col = "darkgreen", lwd = 4, lty = 2)
curve(gsf3, data[3, 3], 1, add = T, col = "darkgreen", lwd = 4, lty = 3)
curve(gsf4, data[4, 3], 1, add = T, col = "darkgreen", lwd = 2, lty = 1)
curve(gsf5, data[5, 3], 1, add = T, col = "darkgreen", lwd = 2, lty = 2)
curve(gsf6, data[6, 3], 1, add = T, col = "darkgreen", lwd = 2, lty = 3)
curve(gsf7, data[7, 3], 1, add = T, col = "darkgreen", lwd = 1, lty = 1)
curve(gsf8, data[8, 3], 1, add = T, col = "darkgreen", lwd = 1, lty = 2)
curve(gsf9, data[9, 3], 1, add = T, col = "darkgreen", lwd = 1, lty = 3)

axis(1, xlim = c(0, 1), pos = 0, lwd = 2)
mtext(expression(italic(s)),side = 1, line = 2.2, cex = 1.3)
axis(2, ylim = c(0, 0.4), pos = 0, lwd = 2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)), side = 2, line = 1.8, cex = 1.3)
legend("topleft", title = "MAP", as.character(c(365, 1825, 3650)), lwd = c(4, 2, 1), col = "darkgreen")
legend("topright", title = expression(italic(k)), as.character(c(0.1, 0.2, 0.4)), lty = 1:3)
text(1*0.04, 0.4*0.055, expression((bold(c))), cex=1.3)
box()
