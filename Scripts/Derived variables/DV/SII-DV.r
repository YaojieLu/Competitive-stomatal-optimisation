
options(digits=22)
source("Scripts/Derived variables/SII-F.r")

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
h <- l*a*LAI/nZ*p
h2 <- l*LAI/nZ*p/1000

# Environmental conditions
ca <- 400
k <- 0.05
MAP <- 1825
gamma <- 1/((MAP/365/k)/1000)*nZ
pkx <- 0.5
h3 <- 25
d <- seq(1, 10, by=1)
env <- as.vector(expand.grid(pkx, h3, d))
colnames(env) <- c("pkx", "h3", "d")
env1 <- data.frame("pkx"=c(0.5, 0.5), "h3"=c(1, 100), "d"=c(5, 5))
env <- rbind(env, env1)
df <- data.frame(wL=numeric(), P50=numeric(), pxmin=numeric(), pxgs50=numeric(), slope=numeric())

# ESS
for(i in 1:nrow(env)){
  pkx <- env[i, 1]
  h3 <- env[i, 2]
  d <- env[i, 3]
  
  wL <- uniroot(optwLif, c(0.1, 0.3), tol=.Machine$double.eps^0.25)$root
  wLL <- wLLf(wL)
  g1 <- gswLf(1, wL)
  f1 <- function(w)gswLf(w, wL)-g1*0.5
  wgs50 <- uniroot(f1, c(wLL, 1), tol=.Machine$double.eps)$root
  
  df[i, 1] <- wL
  df[i, 2] <- P50f(d)
  df[i, 3] <- pxf(wLL, gswLf(wLL, wL), wL)
  df[i, 4] <- pxf(wgs50, gswLf(wgs50, wL), wL)
  df[i, 5] <- (df[i, 3]-pxf(1, gswLf(1, wL), wL))/(psf(wLL)-psf(1))
}

# Save file
res <- cbind(env, df)
colnames(res) <- c("pkx", "h3", "d", "wL", "P50", "pxmin", "pxgs50", "slope")
write.csv(res, "Results/SII-DV.csv", row.names = FALSE)
