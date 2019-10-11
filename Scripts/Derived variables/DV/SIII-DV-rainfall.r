
options(digits=22)
source("Scripts/Derived variables/SIII-F.r")
data   <- read.csv("Results/SIII-DV-rainfall.csv")

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
d <- 5

# Environmental conditions
ca <- 400
k <- c(0.1, 0.2, 0.4)
MAP <- c(365, 1825, 3650)
env <- as.vector(expand.grid(k, MAP))

# Initialize
total <- nrow(env)
df <- data.frame(numeric(total), numeric(total), numeric(total), numeric(total))
pb <- txtProgressBar(min=0, max=total, style=3)

# ESS
for(i in 1:total){
  k <- env[i, 1]
  MAP <- env[i, 2]
  gamma <- 1/((MAP/365/k)/1000)*nZ
  
  x <- try(uniroot(optwLf, c(0.01, 0.3), tol=.Machine$double.eps))
  if(is.numeric(x[[1]])){
    wL <- x$root
    df[i, 1] <- wL
    df[i, 2] <- Psi50fd(d)
    df[i, 3] <- psf(wL)
    g1 <- gswLf(1, wL)
    f1 <- function(w)gswLf(w, wL)-g1*0.5
    w50 <- uniroot(f1, c(wL, 1), tol=.Machine$double.eps)$root
    df[i, 4] <- psf(w50)
  }else{
    df[i, ] <- 100
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# Save file
res <- cbind(env, df)
colnames(res) <- c("k", "MAP", "wL", "P50", "pxmin", "pxgs50")
write.csv(res, "Results/SIII-DV-rainfall.csv", row.names = FALSE)
