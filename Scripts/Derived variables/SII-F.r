
# Soil water retention curve
psf <- function(w)pe*w^(-b)

# From ps to w
wf <- function(ps)(ps/pe)^(-1/b)

# The original PLC(px)
PLCf <- function(px)1-exp(-(-px/d)^c)

# modified PLC
PLCfm1 <- Vectorize(function(px, wL){
  pxL <- psf(wL)
  res <- ifelse(px>pxL, PLCf(pxL)-(PLCf(pxL)-PLCf(px))*pkx, PLCf(px))
  return(res)
})

# P50
P50f <- Vectorize(function(d){
  f1 <- function(px)exp(-(-px/d)^c)-0.5
  res <- uniroot(f1, c(-20, 0), tol=.Machine$double.eps)$root
  return(res)
})

# modified gsmax
gsmaxfm <- function(w, wL){
  # modified PLC
  PLCfm <- function(x)PLCf(pxL)-(PLCf(pxL)-PLCf(x))*pkx
  # modified xylem conductance function
  kxfm <- function(x)kxmax*(1-PLCfm(x))
  
  f1 <- function(x)(ps-x)*h2*kxfm(x)/(h*VPD)
  
  pxL <- psf(wL)
  ps <- psf(w)
  res <- ifelse(pxL<ps, optimize(f1, c(pxL, ps), tol=.Machine$double.eps, maximum=T)$objective, 0)
  return(res)
}

# modified minimum xylem water potential
pxminfm <- function(w, wL){
  # modified PLC
  PLCfm <- function(x)PLCf(pxL)-(PLCf(pxL)-PLCf(x))*pkx
  # modified xylem conductance function
  kxfm <- function(x)kxmax*(1-PLCfm(x))
  
  f1 <- function(x)(ps-x)*h2*kxfm(x)/(h*VPD)
  
  pxL <- psf(wL)
  ps <- psf(w)
  res <- ifelse(pxL<ps, optimize(f1, c(pxL, ps), tol=.Machine$double.eps, maximum=T)$maximum, ps)
  return(res)
}

# xylem water potential function
pxf <- Vectorize(function(w, gs, wL){
  # modified PLC
  PLCfm <- function(x)PLCf(pxL)-(PLCf(pxL)-PLCf(x))*pkx
  # modified xylem conductance function
  kxfm <- function(x)kxmax*(1-PLCfm(x))
  
  f1 <- function(x)(ps-x)*h2*kxfm(x)-h*VPD*gs
  
  pxL <- psf(wL)
  ps <- psf(w)
  pxmin <- pxminfm(w, wL)
  res <- ifelse(pxmin<ps, uniroot(f1, c(pxmin, ps), tol=.Machine$double.eps)$root, ps)
  return(res)
})

# Photosynthesis rate
Af <- function(gs)LAI*1/2*(Vcmax+(Km+ca)*gs-Rd-((Vcmax)^2+2*Vcmax*(Km-ca+2*cp)*gs+((ca+Km)*gs+Rd)^2-2*Rd*Vcmax)^(1/2))

# modified refilling cost
mfm <- function(w, gs, wL){
  # modified PLC
  PLCfm <- function(x)PLCf(pxL)-(PLCf(pxL)-PLCf(x))*pkx
  
  pxL <- psf(wL)
  px <- pxf(w, gs, wL)
  res <- h3*(PLCfm(px)-PLCfm(0))
  return(res)
}

# modified B(w, gs)
Bfm <- function(w, gs, wL)Af(gs)-mfm(w, gs, wL)

# Subspace ESS
gswLf <- Vectorize(function(w, wL){
  Bfm1 <- function(gs)Bfm(w, gs, wL)
  gsmaxfm1 <- function(w)gsmaxfm(w, wL)
  
  res <- ifelse(0<gsmaxfm1(w), optimize(Bfm1, c(0, gsmaxfm1(w)), tol=.Machine$double.eps, maximum=T)$maximum, 0)
  return(res)
})

# Subspace ESS gs(ps)
gswLpsf <- function(ps, wL){
  w <- wf(ps)
  res <- gswLf(w, wL)
  return(res)
}

# Subspace ESS PLC(ps)
ESSPLCpsf <- function(ps, wL){
  w <- wf(ps)
  px <- pxf(w, gswLf(w, wL), wL)
  res <- PLCf(px)
  return(res)
}

# Subspace ESS A(w)
AwLf <- function(w, wL)Af(gswLf(w, wL))

# Subspace ESS B(w)
BwLf <- function(w, wL)Bfm(w, gswLf(w, wL), wL)

# ESS g1(ps)
ESSg1psf <- Vectorize(function(ps, wL){
  f1 <- function(w)psf(w)-ps
  w <- uniroot(f1, c(0.001, 1), tol=.Machine$double.eps)$root
  res <- sqrt(VPD*100)*(ca*gswLf(w, wL)/(a*AwLf(w, wL))-1)
  return(res)
})

# Lower bound of the domain of the ESS
wLLf <- Vectorize(function(wL){
  BwLf1 <- Vectorize(function(w)BwLf(w, wL))
  res <- uniroot(BwLf1, c(wL, 1), tol=.Machine$double.eps)$root
  return(res)
})

# Average B for invader
averBif <- function(wLi, wLr){
  wLLr <- wLLf(wLr)
  wLLi <- wLLf(wLi)
  
  gsmaxfmr <- function(w)gsmaxfm(w, wLr)
  gsmaxfmi <- function(w)gsmaxfm(w, wLi)
  
  gswLfr <- Vectorize(function(w)ifelse(w<wLLr, 0, gswLf(w, wLr)))
  gswLfi <- Vectorize(function(w)ifelse(w<wLLi, 0, gswLf(w, wLi)))
  
  Evf <- function(w)h*VPD*gswLfr(w)
  Lf <- function(w)Evf(w)+w/100
  rLf <- function(w)1/Lf(w)
  integralrLf <- Vectorize(function(w)integrate(rLf, w, 1, rel.tol=.Machine$double.eps^0.25)$value)
  fnoc <- function(w)1/Lf(w)*exp(-gamma*w-k*integralrLf(w))# Stochastic model of soil moisture
  
  f1 <- Vectorize(function(w)Bfm(w, gswLfi(w), wLi)*fnoc(w))
  res <- integrate(f1, wLLi, 1, rel.tol=.Machine$double.eps^0.25)$value
  #message(wLr, " ", wLi, " ", res)
  return(res)
}

# Average B in monoculture
averf <- function(wL){
  wLL <- wLLf(wL)
  
  gswLf1 <- Vectorize(function(w)ifelse(w<wLL, 0, gswLf(w, wL)))
  
  Evf <- function(w)h*VPD*gswLf1(w)
  Lf <- function(w)Evf(w)+w/100
  rLf <- function(w)1/Lf(w)
  integralrLf <- Vectorize(function(w)integrate(rLf, w, 1, rel.tol=.Machine$double.eps^0.4)$value)
  fnoc <- function(w)1/Lf(w)*exp(-gamma*w-k*integralrLf(w))
  browser()
  res1 <- integrate(fnoc, 0, 1, rel.tol=.Machine$double.eps^0.4)#$value
  cPDF <- 1/res1$value
  
  fA <- Vectorize(function(w)Af(gswLf1(w))*cPDF*fnoc(w))
  resA <- integrate(fA, wLL, 1, rel.tol=.Machine$double.eps^0.4)#$value
  fE <- Vectorize(function(w)Evf(w)*cPDF*fnoc(w))
  resE <- integrate(fE, wLL, 1, rel.tol=.Machine$double.eps^0.4)#$value
  res <- c(resA$value, resE$value*500*365)
  return(res)
}

# ESS derivation
optwLif <- Vectorize(function(wLr){
  averBif1 <- Vectorize(function(wLi)averBif(wLi, wLr))
  optwLi <- optimize(averBif1, c(0.1, 0.3), tol=.Machine$double.eps^0.25, maximum=T)
  res <- optwLi$maximum-wLr
  message(wLr, " ", optwLi$maximum)
  return(res)
})
