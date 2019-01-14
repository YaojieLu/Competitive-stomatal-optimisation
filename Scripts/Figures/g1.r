
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-0.5, 10), cex.lab=1.3)
for(i in 1:nrow(pars))curve(pars[i, "a"]*exp(pars[i, "b"]*x),
                            from=pars[i, "LWP.min"], to=0, add=TRUE, col = "gray60", lwd=1)
points(datag1SIL, type="l", col=Cols[1])
points(datag1SIH, type="l", col=Cols[1], lty=2)
points(datag1SIII, type="l", col=Cols[3])

axis(1, xlim=c(-10, 0), pos=-0.5, lwd=2, at=c(-10, -5, 0))
mtext(expression(psi[s]~(MPa)), side=1, line=3, cex=1.3)
axis(2, ylim=c(0, 10), pos=-10, lwd=2, at=c(0, 5, 10))
mtext(expression(italic(g[1])~(kPa^0.5)), side=2, line=2, cex=1.3)
text(-10*0.965, -0.5+10.5*0.05, "a", cex=1.3)
legend("topright", c("Zhou et al. 2014"), col = "gray60", lwd=1, lty=1)
legend("topleft", c(expression(paste(italic(p[k]==100), ",  ", beta==1)),
                    expression(paste(italic(p[k]==100), ",  ", beta==100)),
                    expression(paste(italic(p[k]==0)))), lty=c(1, 2, 1), col=c("purple", "purple", "darkgreen"))
box()
