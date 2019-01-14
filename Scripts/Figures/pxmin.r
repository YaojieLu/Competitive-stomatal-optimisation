
Colsd <- c("lightblue", "lightpink")

plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-15, 0), ylim=c(-15, 0),
     cex.lab=1.3, col=Colsd[1])
points(dataAng, type="p", col=Colsd[1], pch=1, lwd=1)
lines(dataAng$Psi50, predict(fitAng), col=Colsd[1], lwd=1)
points(dataGym, type="p", col=Colsd[2], pch=2, lwd=1)
lines(dataGym$Psi50, predict(fitGym), col=Colsd[2], lwd=1)
points(subset(dataDVSIII, d<11, select=c("P50", "pxmin")), type="l", col=Cols[3])
points(subset(dataDVSI, LAI==3 & h3==1 & d<11, select=c("P50", "pxmin")), type="l", col=Cols[1])
points(subset(dataDVSI, LAI==3 & h3==100 & d<11, select=c("P50", "pxmin")), type="l", lty=2, col=Cols[1])

axis(1, xlim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(psi[x50]~(MPa)),side=1,line=3, cex=1.3)
axis(2, ylim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(psi[xclose]~(MPa)),side=2,line=2, cex=1.3)
text(-15*0.965, -15*0.05, "d", cex=1.3)
legend("bottomleft", title=expression(Choat~italic(et~al.)~2012), c("Angiosperm", "Gymnosperm"), lwd=1, pch=c(1, 2), lty=c(1, 1), col=Colsd[1:2])
box()
