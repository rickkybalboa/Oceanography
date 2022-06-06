# Functions used in BODC plotting script
# R. Anderson (unless otherwise noted) - July 2017. 

PressToDepth <- function(filename){
    
    # PRESSTODEPTH  Converts pressure from CTD readings into water depth.
    #
    # Uses formula from UNESCO Technical Papers in Marine Science No. 44. 
    # This is an empirical formula that takes compressibility (that is, density)
    # into account. An ocean water column at 0 °C (t = 0) and 35 PSU (s = 35) 
    # is assumed.
  
    # Created by Martyn Roberts - 18/08/2016.
    
    
    #setwd("~/Oceanography/Sponge_Project/Data")
    latitude <- Lats(filename)
    
    pressure <- ctd.i[,grep("PRES",colnames(ctd.i))]
    
    waterDepth <- pressure*0L # create a matrix of zeroes to hold data
    g <- pressure*0L
    
    # From UNESCO Technical Papers in Marine Science No. 44
    for (i in c(1:length(pressure))){
      x <- sin(latitude/(360/(2*pi)))^2
      g[i] <- 9.780318 * ( 1.0 + (( 5.2788e-3  + (2.36e-5  * x)) * x )) + (1.092e-6  * pressure[i] )
      waterDepth[i] <- ((((-1.82e-15  * pressure[i] + 2.279e-10 ) * pressure[i] - 2.2512e-5 ) * pressure[i] + 9.72659) * pressure[i]) / g[i]
    }
    
    return(waterDepth)
    #setwd("..")
  }
  
  #**************************************************
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  Lats <- function(filename) {
    
  
    
    # LATS  Converts BODC co-ords to decimal latitude (required by PRESS2DEPTH)
    #
    # Uses regular expression search to extract lat. data from BODC
    # files, splits into integer and fractional parts
    # (i.e., 12.345 -> xx.yyy), and processes/recombines to form 
    # decimal latitude.
    
    setwd("~/Oceanography/Sponge_Project/Data")
    
    #Extracts BODC co-ordinate data using regular expression search, 
    text = read.table(filename, sep = "\t", stringsAsFactors =  FALSE)
    bodc.coords <- str_match(text,"\\d+\\w+\\d+.\\d\\w+\\d+\\w+\\d+.\\d+\\w+")
    rm(text)
    
    # string search to retrieve integer and fractional part of decimal latitude
    regexp.int <- "..."  
    regexp.frac <- "\\d+[.]\\d+"  
    lat.int <- as.numeric(str_extract(bodc.coords, regexp.int))
    lat.frac <- as.numeric(str_extract(bodc.coords, regexp.frac))
    lat.frac <- lat.frac/60 
    
    latitude = lat.int + lat.frac
    
    return(latitude)
    return(bodc.coords)
  }
  #setwd("..")
  #**************************************************
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  PoTemp <- function(filename) {
    
    # POTEMP  Searches for potential temperature field in data, converts temp
    # recorded by CTD to pot. temp if not found.
  
  setwd("\\\\fs-home-n/home-014/ossa25/My Documents/Sponge_Project/Data")
  
  if("POTMCV01" %in% colnames(ctd.i))
  {
    cat("All OK.\n");
  } else {
    cat("No pot.temp field found. Converting to pot.temp...\n");
    pressure <- ctd.i[,grep("PRES",colnames(ctd.i))]
    temp     <- ctd.i[,grep("TEMP",colnames(ctd.i))]
    sal      <- ctd.i[,grep("SAL",colnames(ctd.i))]
    pot.temp <- temp*0L  # placeholder vector
    
    for (j in c(1:length(sal))) {
                              # reference pressure = 10 dbar
      pot.temp[j] <- Theta(pressure[j], temp[j], sal[j], 10)
    }
    
    rm(pressure,temp,sal)
    
    return(pot.temp)
    }
  }
  setwd("..")
  #**************************************************
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  Theta <- function(p,t,s,p0) {
    
    # THETA  Computes local potential temperature at reference pressure.
    # 
    # PTEMP = THETA(P,T,S,P0) is the local potential temperature
    # at reference pressure P0 using Bryden 1973 polynomial for
    # adiabatic lapse rate and Runge-Kutta fourth order integration
    # algorithm.
    # 
    # Units:
    # Pressure        P, P0   dbar
    # Temperature     T       degC IPTS-78
    # Salinity        S       PSU  PSS-78
    # Defaults:
    #   P0		0 dbar
    # 
    # Checkvalue:
    #   theta(10000,40,40,0) = 36.89072
    #  
    #  18/02/93, C. Mertens, IfM Kiel, changed to Matlab
    #  added default p0=0dbar	G.Krahmann, IfM Kiel, Mar 1996,
    
    p = p/10 ; 
    p0 = p0/10 ; 
    h = p0 - p ;
    x = h*atg(p,t,s) ;
    t = t + 0.5*x ;
    q = x ;
    p = p + 0.5*h ;
    x = h*atg(p,t,s) ;
    t = t + 0.29289322*(x - q) ;
    q = 0.58578644*x + 0.121320344*q;
    x = h*atg(p,t,s) ;
    t = t + 1.707106781*(x - q) ;
    q = 3.414213562*x - 4.121320344*q ;
    p = p + 0.5*h ;
    x = h*atg(p,t,s) ;
    ptemp = t + (x - 2*q)/6 ;
    
    return(ptemp)
  } 
  
  #**************************************************
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  atg <- function(p,t,s) {
    #ATG Computes adiabatic temperature gradient (required by THETA).
    #       A = ATG(P,T,S)
    
    #       VAX 11/750      1983    J.HOLTORFF
    #       18/02/93, C. Mertens, IfM Kiel, changed to Matlab
    
    s = s-35.0 ;
    
    # a = (((-2.1687E-13*t + 1.8676E-11)*t - 4.6206E-10)*p
    #      + (( 2.7759E-10*t - 1.1351E-08)*s
    #         + ((-5.4481E-12*t + 8.7330E-10)*t - 6.7795E-08)*t + 1.8741E-06))*p
    # +  (-4.2393E-07*t + 1.8932E-05)*s
    # + (( 6.6228E-09*t - 6.8360E-07)*t + 8.5258E-05)*t + 3.5803E-04 ;
    
    ## MUST END LINES WITH OPERATOR FOR CALCULATION TO COMPLETE CORRECTLY! ###
    
    a = (((-2.1687E-13*t + 1.8676E-11)*t - 4.6206E-10)*p + 
           (( 2.7759E-10*t - 1.1351E-08)*s + ((-5.4481E-12*t + 8.7330E-10)*t - 
                                                6.7795E-08)*t + 1.8741E-06))*p + (-4.2393E-07*t + 1.8932E-05)*s + 
      (( 6.6228E-09*t - 6.8360E-07)*t + 8.5258E-05)*t + 3.5803E-04 ;
    
    
    return(a)
  }
  
  #**************************************************
  
  MultiPlot <- function(files,bodc.coords) {
  
  for (f in files) {
    ctd.i <- ctd[[f]]
    t <- ctd.i[,grep("POTMC",colnames(ctd.i))]
    s <- ctd.i[,grep("SAL",colnames(ctd.i))]
    
    png(file=paste("~/Oceanography/Sponge_Project/Data/",region,
                   "/Images/","BODC-CTD-",f,".png",sep=""),
        width=5, height=4, units="in", res=1200, pointsize=10)
    
    plot(s,t)
    drawIsopycnals()
    title(f, sub = bodc.coords[f])
    dev.off()
  }
    
  }
  
  #####################################################
  
  AvgPlot <- function(ctd.avg,geodiaH.sal,geodiaH.temp, geodiaP.sal, geodiaP.temp, xlimits, ylimits) {
  
  png(file=paste0("~/Oceanography/Sponge_Project/Data/",region,"/AvgPlot_",region,".png"),
        width=5, height=4, units="in", res=1200, pointsize=10)
    
  plot(ctd.avg$SAL,ctd.avg$TEMP,
       #xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1),
       #ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
       xlim = xlimits,
       ylim = ylimits,
       xlab = "Salinity (psu)",
       ylab = "Temperature ("~degree~"C)",
       "l"
  )
  par(new=T)
  plot(geodiaH.sal,geodiaH.temp, 
       pch = 21,
       # xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1), 
       # ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
       xlim = xlimits,
       ylim = ylimits,
       xlab = "", ylab = "",
       col = "red",
       bg  = "red",
       cex = 1.25
  )
  par(new=T)
  plot(geodiaP.sal,geodiaP.temp, 
       pch = 21,
       # xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1), 
       # ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
       xlim = xlimits,
       ylim = ylimits,
       xlab = "", ylab = "",
       col = "blue",
       bg  = "blue"
       #cex = 1.25
  )
  title(paste0("",region," Region TSPlot (stations averaged)"))
  legend( 'topright'
          , c("G.Hentscheli","G.Phlegraei/Parva")
          , pch = 21
          , col = c("red","blue")
          , pt.bg = c("red","blue")
          , bty ='n'
          , cex = .75
  )
  drawIsopycnals()
  dev.off()
  }
  
  ######################################################################
  
  GeodiaExtract <- function(geodiaData,latLimits,longLimits) {
  
  geodia <- geodiaData  
  x <- data.frame(lapply(geodia, function(x) x <- NA))
  
  lowerLatLimit  <- latLimits[1]
  upperLatLimit  <- latLimits[2]
  lowerLongLimit <- longLimits[1]
  upperLongLimit <- longLimits[2]
  
  for (i in 1:length(geodia[[1]])) {
    if (is.na(geodia[i,grep("Lat",names(geodia),ignore.case = T)]) == TRUE) {
      next
    }else if (geodia[i,grep("Lat",names(geodia),ignore.case = T)] >= lowerLatLimit && geodia[i,grep("Lat",names(geodia),ignore.case = T)] <= upperLatLimit &&
              geodia[i,grep("Lon",names(geodia),ignore.case = T)] >= lowerLongLimit && geodia[i,grep("Lon",names(geodia),ignore.case = T)] <= upperLongLimit) {
      x[i,] <- geodia[i,]
    }else { 
      next
    }
  }
  return(na.omit(x))
  }
  
  #######################################################################
  AvgPlotGLOBENV <- function(ctd.avg,geodiaH.sal,geodiaH.temp, geodiaP.sal, geodiaP.temp, xlimits, ylimits) {
    
    png(file=paste0("~/Oceanography/Sponge_Project/Data/",region,"/AvgPlot_",region,"_GLOBENV.png"),
        width=5, height=4, units="in", res=1200, pointsize=10)
    
    plot(ctd.avg$SAL,ctd.avg$TEMP,
         #xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1),
         #ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
         xlim = xlimits,
         ylim = ylimits,
         xlab = "Salinity (psu)",
         ylab = "Temperature ("~degree~"C)",
         "l"
    )
    par(new=T)
    plot(geodiaH.sal,geodiaH.temp, 
         pch = 21,
         # xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1), 
         # ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
         xlim = xlimits,
         ylim = ylimits,
         xlab = "", ylab = "",
         col = "red",
         bg  = "red",
         cex = 1.25
    )
    par(new=T)
    plot(geodiaP.sal,geodiaP.temp, 
         pch = 21,
         # xlim = c(min(ctd.avg$SAL)-.2, max(ctd.avg$SAL)+0.1), 
         # ylim = c(min(ctd.avg$TEMP)-0.5, max(ctd.avg$TEMP)+0.5),
         xlim = xlimits,
         ylim = ylimits,
         xlab = "", ylab = "",
         col = "blue",
         bg  = "blue"
         #cex = 1.25
    )
    title(paste0("",region," Region TSPlot (stations averaged) GLOBENV DATA"))
    legend( 'topright'
            , c("G.Hentscheli","G.Phlegraei/Parva")
            , pch = 21
            , col = c("red","blue")
            , pt.bg = c("red","blue")
            , bty ='n'
            , cex = .75
    )
    drawIsopycnals()
    dev.off()
  }
  