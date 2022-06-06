# Plot TS Diagrams From GO Sars 2016.
# Created by Martyn Roberts - 18/08/2016.
# Based on a script produced by Andy Davies whilst onboard (see R.R).

#####
# SET WORKING DIRECTORY
#####

# Martyn Lenovo on Transcend GO Sars Ext Hard Disk
setwd("E:\\GO_SARS_2016\\CTD_Profiles")

#####
# LOAD LIBRARIES
#####

# Load libaries
packages <- c("oce","ggplot","gsw","akima","ocedata","reshape2","zoo","plyr","Unicode")
sapply(packages, require, character.only = TRUE); rm(packages)
options(oceUnitBracket="(")

#####
# READ IN DATA.
#####

# Read CTD Data
# 
# Three sets of data, Reference/SVP, Transect 1 (across seamount), Transect 2 (along seamount).
# 
# CTD Stations
# List ID   Station name        Note                  Filename
# 1         GS16A-01-CTD-01     Reference and SVP     472.cnv
# 2         GS16A-14-CTD-02     Transect 1            473.cnv
# 3         GS16A-15-CTD-03     Transect 1            474.cnv
# 4         GS16A-16-CTD-04     Transect 1            475.cnv
# 5         GS16A-17-CTD-05     Transect 1            476.cnv
# 6         GS16A-18-CTD-06     Transect 1            477.cnv
# 7         GS16A-24-CTD-07     Transect 2            478.cnv
# 8         GS16A-25-CTD-08     Transect 2            479.cnv
# 9         GS16A-26-CTD-09     Transect 2            480.cnv
# 10        GS16A-27-CTD-10     Transect 2            481.cnv
# 11        GS16A-28-CTD-11     Transect 2            482.cnv

ctd <- read.ctd.sbe("*.cnv")
# Check read is OK
for(i in c(1:length(ctd))) { str(ctd[[i]]@metadata$station, 1) }; rm(i)
# Modify station name to reflect our station name rather than filename
stations <- c("GS16A-01-CTD-01","GS16A-14-CTD-02","GS16A-15-CTD-03","GS16A-16-CTD-04", 
              "GS16A-17-CTD-05", "GS16A-18-CTD-06", "GS16A-24-CTD-07", "GS16A-25-CTD-08",
              "GS16A-26-CTD-09", "GS16A-27-CTD-10", "GS16A-28-CTD-11")
n = 1; for(i in stations) { ctd[[n]]@metadata$station <- i; n = n+1}; rm(stations, n, i)




for (k in 1:11) {
  # Fix Station number
  ctd.i <- ctdDecimate(ctd[[k]], p=1, method="boxcar")
  ctd.i@metadata$startTime <- ""
  maxD = ctd.i@metadata$waterDepth
  
  # Convert pressure to depth (Sea-Bird uses the formula in UNESCO Technical Papers in Marine Science No. 44. This is an empirical formula that takes compressibility (that is, density) into account. An ocean water column at 0 °C (t = 0) and 35 PSU (s = 35) is assumed.)
  x <- sin(ctd.i@metadata$latitude/(360/(2*pi)))^2
  g <- 9.780318 * ( 1.0 + (( 5.2788e-3  + (2.36e-5  * x)) * x )) + (1.092e-6  * ctd.i@data$pressure)
  ctd.i@data$waterdepth <- ((((-1.82e-15  * ctd.i@data$pressure + 2.279e-10 ) * ctd.i@data$pressure - 2.2512e-5 ) * ctd.i@data$pressure + 9.72659) * ctd.i@data$pressure) / g
  
  # Remove spurious data points (and top 100m) before plotting.
  # CTD1.
  if (k==1){
    # Sal.
    ctd.i@data$salinity[2440] <- NA; ctd.i@data$salinity[2441] <- NA; ctd.i@data$salinity[2442] <- NA; ctd.i@data$salinity[2444] <- NA; ctd.i@data$salinity[2472] <- NA; ctd.i@data$salinity[2473] <- NA; ctd.i@data$salinity[2476] <- NA; ctd.i@data$salinity[2481] <- NA
    # Sal2.
    ctd.i@data$salinity2[2440] <- NA; ctd.i@data$salinity2[2441] <- NA; ctd.i@data$salinity2[2442] <- NA; ctd.i@data$salinity2[2444] <- NA; ctd.i@data$salinity2[2472] <- NA; ctd.i@data$salinity2[2473] <- NA; ctd.i@data$salinity2[2476] <- NA; ctd.i@data$salinity2[2481] <- NA
    # Temp.
    ctd.i@data$temperature[2440] <- NA; ctd.i@data$temperature[2441] <- NA; ctd.i@data$temperature[2442] <- NA; ctd.i@data$temperature[2444] <- NA; ctd.i@data$temperature[2472] <- NA; ctd.i@data$temperature[2473] <- NA; ctd.i@data$temperature[2476] <- NA; ctd.i@data$temperature[2481] <- NA
    # Temp2.
    ctd.i@data$temperature2[2440] <- NA; ctd.i@data$temperature2[2441] <- NA; ctd.i@data$temperature2[2442] <- NA; ctd.i@data$temperature2[2444] <- NA; ctd.i@data$temperature2[2472] <- NA; ctd.i@data$temperature2[2473] <- NA; ctd.i@data$temperature2[2476] <- NA; ctd.i@data$temperature2[2481] <- NA
  }
  # CTD4
  if (k==4){
    # Sal.
    ctd.i@data$salinity[658] <- NA
    # Sal2.
    ctd.i@data$salinity2[658] <- NA
    # Temp.
    ctd.i@data$temperature[658] <- NA
    # Temp2.
    ctd.i@data$temperature2[658] <- NA
  }
  #CTD10
  if (k==10){
    # Sal.
    ctd.i@data$salinity[313] <- NA
    # Sal2.
    ctd.i@data$salinity2[313] <- NA
    # Temp.
    ctd.i@data$temperature[313] <- NA
    # Temp2.
    ctd.i@data$temperature2[313] <- NA
  }
  
  # Remove top 100 m, all stations (actually setting to NaN.
  # Sal.
  ctd.i@data$salinity[1:100] <- NA
  # Sal2.
  ctd.i@data$salinity2[1:100] <- NA
  # Temp.
  ctd.i@data$temperature[1:100] <- NA
  # Temp2.
  ctd.i@data$temperature2[1:100] <- NA
  
  # Plotting TS diagrams.
  
  png(file=paste(getwd(),"GS16A-01-CTD-",k,"_TSDiag.png",sep=""), width=3.5, height=3.5, units="in", res=1200, pointsize=10) # L and O 1-column width.
  
  # This is a bodge because PlotTS plots isopycnals on top of data (me no like!). So, here I plot twice - once with isopycnals showing but the data as white circles, then again on top (add) with proper data markers but the isopycnals switched off (nlevels="").
  plotTS(subset(ctd.i, ctd.i@data$waterdepth < maxD), cex=1, inSitu=FALSE,  xlab=expression(paste("Salinity, ", "S", " (psu)")), type="p", pch=19, lwd=0.5, pt.bg="darkgrey", grid=FALSE, lwd.rho = 1, lty.rho = "dashed", Tlim=c(-1,6), Slim=c(34.8,35.2), yaxs = 'i', xaxs = 'i', col="white", ylab = expression(paste("Potential Temperature, ", theta , " ("*degree*"C)")), referencePressure = 0)
  plotTS(subset(ctd.i, ctd.i@data$waterdepth < maxD), cex=0.3, inSitu=TRUE, xlab=expression(paste("Salinity, ", "S", " (psu)")), type="p", pch=20, lwd=0.5, pt.bg="darkgrey", grid=FALSE, lwd.rho = 1, lty.rho = "dashed", Tlim=c(-1,6), Slim=c(34.8,35.2), yaxs = 'i', xaxs = 'i',levels=c(), add=TRUE, ylab = expression(paste("Potential Temperature, ", theta , " ("*degree*"C)")), referencePressure = 0)
  depths_to_label1 <- c(101,201,301,1001,2001)
  depths_to_label2 <- c(401,501,601) # Split these up so can have diff. positions.
  text(ctd.i@data$salinity[depths_to_label1], ctd.i@data$temperature[depths_to_label1], labels=paste(round_any(ctd.i@data$waterdepth[depths_to_label1],100,f=ceiling),"m"), cex= 0.7, pos=2) #Careful, rounding up here to nearest 100 for illustration only.
  text(ctd.i@data$salinity[depths_to_label2], ctd.i@data$temperature[depths_to_label2], labels=paste(round_any(ctd.i@data$waterdepth[depths_to_label2],100,f=ceiling),"m"), cex= 0.7, pos=4) #Careful, rounding up here to nearest 100 for illustration only.
  # Add water mass points/lines and labels.
  
  points(34.88, 0.5, pch=4, lwd=2, col="steelblue2") #NwArIW.
  points(34.92, -0.5,pch=4, lwd=2, col="steelblue2") #uNwDW.
  lines(c(35,35.075),c(2,3.5), lwd = 2, col="steelblue2")  #NwAtW.
  lines(c(35.075,35.15),c(3.5,5), lty="12", lwd=2, col="steelblue2")  #NwAtW (cont.). "11" is a high density dotted line.
  #lines(c(35,35.3),c(2,8), lwd = 3)  # Test line running from Hopkins min NwAtW values to min NAtW. Usually comment out.
  text(34.88, 0.5, labels="NwArIW", adj = c(1.05, -0.5), cex=0.9, font=2, col="black")
  text(34.92, -0.5, labels="uNwDW", adj = c(-0.05, 1.5), cex=0.9, font=2, col="black")
  text(35.0375, 2.75, labels="NwAtW", adj = c(-0.05, 1.5), cex=0.9, font=2, col="black")
  dev.off()
}
