
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-10, 0), cex.lab=1.3)
curve(0.49*x-0.42, -7, -1, lty=1, add=T, lwd=1, col = "gray60")
points(subset(dataDVSIII, d<11, select=c("P50", "pxgs50")), type="l", col=Cols[3])
points(subset(dataDVSI, LAI==3 & h3==1 & d<11, select=c("P50", "pxgs50")), type="l", col=Cols[1])
points(subset(dataDVSI, LAI==3 & h3==100 & d<11, select=c("P50", "pxgs50")), type="l", lty=2, col=Cols[1])

axis(1, xlim=c(-10, 0), pos=-10, lwd=2, at=c(-10, -5, 0))
mtext(expression(psi[x50]~(MPa)), side=1, line=3, cex=1.3)
axis(2, ylim=c(-10, 0), pos=-10, lwd=2, at=c(-10, -5, 0))
mtext(expression(psi[x*", "*italic(g[s]*50)]~(MPa)), side=2, line=2, cex=1.3)
text(-10*0.965, -10*0.05, "c", cex=1.3)
legend("bottomright", c("Klein 2014"), lty=1, lwd=1, col = "gray60")
box()
