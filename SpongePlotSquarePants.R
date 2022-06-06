#PLOT TS DATA FOR DEEP SEA SPONGE DISTRIBUTION PROJECT

#####
# SET WORKING DIRECTORY
#####

#
setwd("\\\\fs-home-n/home-014/ossa25/My Documents/R/TS_Script")

#####
# LOAD LIBRARIES
#####

# Load Libraries
library("oce","ggplot2")

#####
# READ IN DATA.
#####

ctd <- read.table("test.txt", header=TRUE)

ctd$CPHLPR01 <- NA

#########
# Pressure to depth conversion
#########

pressure = ctd[,9]

waterDepth <- pressure*0L #create a matrix of zeroes of same dimensions as 'pressure' object
g <- pressure*0L

# From UNESCO Technical Papers in Marine Science No. 44
for (i in c(1:length(pressure))){
  x <- sin(63.11533/(360/(2*pi)))^2
  g[i] <- 9.780318 * ( 1.0 + (( 5.2788e-3  + (2.36e-5  * x)) * x )) + (1.092e-6  * pressure[i] )
  waterDepth[i] <- ((((-1.82e-15  * pressure[i] + 2.279e-10 ) * pressure[i] - 2.2512e-5 ) * pressure[i] + 9.72659) * pressure[i]) / g[i]
  
  #ctd$waterDepth <-
}



#plotTS(subset(ctd.i, ctd.i@data$waterDepth < maxD), cex=1, inSitu=FALSE,  xlab=expression(paste("Salinity, ", "S", " (psu)")), type="p", pch=19, lwd=0.5, pt.bg="darkgrey", grid=FALSE, lwd.rho = 1, lty.rho = "dashed", Tlim=c(-1,6), Slim=c(34.8,35.2), yaxs = 'i', xaxs = 'i', col="white", ylab = expression(paste("Potential Temperature, ", theta , " ("*degree*"C)")), referencePressure = 0)



#splice <- ctd[,2:4]



