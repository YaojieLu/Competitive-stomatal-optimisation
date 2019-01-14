
options(digits=22)
source("Scripts/Derived variables/SIII-F.r")

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
d <- 5

# Define function
# Best I given R
f1 <- function(wLr){
  averAirelf1 <- Vectorize(function(wLi)averAirelf(wLi, wLr))
  optwLi <- optimize(averAirelf1, c(0.1, 0.3), tol=.Machine$double.eps^0.3, maximum=T)
  res <- optwLi$maximum
  return(res)
}
# B_R = B_I at wLr not equal wLi
f2 <- function(wLr, B_r){
  ft <- function(w)(averAirelf(w, wLr) - B_r)^2
  I0 <- optimize(ft, c(Imax, wLibound), tol=.Machine$double.eps^0.5)
  res <- I0$minimum
  return(res)
}
# Initialize
ESS <- psf(0.152775311364286)
x_p <- seq(ESS - 5.9, ESS + 5.9, by = 0.1)
x_w <- wf(x_p)
y <- data.frame("R" = x_w, "Imax" = x_w, "I0" = x_w)

for(i in 1:length(x_w)){
  print(length(x_w) - i)
  wLr <- y[i, 1]
  Imax <- f1(wLr)
  wLibound = ifelse(Imax > wLr, 0.9, 0.1)
  B_r <- averAirelf(wLr, wLr)
  I0 <- f2(wLr, B_r)
  y[i, 1] <- psf(wLr)
  y[i, 2] <- psf(Imax)
  y[i, 3] <- psf(I0)
}

# Save file
write.csv(y, "Results/SIII-grid_search_test.csv", row.names = FALSE)
