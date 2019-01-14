
# Soil water potential
psf <- function(w)pe*w^(-b)

# Minimum xylem water potential
pxminfI <- function(w){
  ps <- psf(w)
  f1 <- function(px)(ps-px)*h2*kxf(px) # Plant water supply
  res <- optimize(f1, c(-40, 0), tol=.Machine$double.eps, maximum=T)$maximum
  return(res)
}

# Xylem water potential
pxfI <- Vectorize(function(w, gs){
  ps <- psf(w)
  pxmin <- pxminfI(w)
  f1 <- function(px)((ps-px)*h2*kxf(px)-h*VPD*gs)^2 # Plant water supply = plant water demand
  res <- ifelse(pxmin<ps, optimize(f1, c(pxmin, ps), tol=.Machine$double.eps)$minimum, ps)
  return(res)
})

# PLC
PLCf <- function(px)1-exp(-(-px/d)^c)