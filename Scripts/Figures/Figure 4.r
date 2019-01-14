
library(nlme)
library(doBy)

# Data
datag1SIL <-  read.csv("Results/SI-g1L.csv")
datag1SIH <-  read.csv("Results/SI-g1H.csv")
datag1SIII <- read.csv("Results/SIII-g1.csv")
dataDVSI <- read.csv("Results/SI-DV.csv")
dataDVSII <- read.csv("Results/SII-DV.csv")
dataDVSIII <- read.csv("Results/SIII-DV.csv")
# a
dataa <- read.csv("Data/Zhou 2014.csv")
# b
rawdatab <- read.csv("Data/Martinez-vilalta 2014.csv")
datab <- data.frame(P50=rawdatab$P50.Mpa, sigma=rawdatab$sigma)
datab <- datab[order(datab$P50), ]
# c
datac <- read.csv("Data/Choat2012.csv")
dataAng <- subset(datac, Type=="Angiosperm", select=c("Psi50", "Psimin"))
dataAng <- dataAng[order(dataAng$Psi50), ]
dataGym <- subset(datac, Type=="Gymnosperm", select=c("Psi50", "Psimin"))
dataGym <- dataGym[order(dataGym$Psi50), ]

# Regression
# a
fits <- nlsList(g1 ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=dataa)
rangefun <- function(x)setNames(range(x), c("min", "max"))
ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=dataa)
pars <- cbind(ran, as.data.frame(coef(fits)))
# b
fit <- nls(sigma ~ a*(-P50)^b+c, data=datab, start=list(a=0.8459, b=0.1261, c=-0.1320),
           control=c(minFactor=1e-5))
# c
fitAng <- nls(Psimin ~ -a*Psi50+b, data=dataAng, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))
fitGym <- nls(Psimin ~ -a*Psi50+b, data=dataGym, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))

# Figure
Cols <- c("purple", "orange", "darkgreen")
windows(8*7/4, 6*7/4)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 4, 0.6, 0.5), mfrow=c(2, 2))
source("Scripts/Figures/g1.r")
par(yaxs="r")
source("Scripts/Figures/Slope.r")
par(yaxs="i")
source("Scripts/Figures/pxgs50.r")
source("Scripts/Figures/pxmin.r")

# Save file
dev.copy2pdf(file = "Figures/Figure 4.pdf")
