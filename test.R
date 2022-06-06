###################################################################
# PLOT TS DATA FOR DEEP SEA SPONGE DISTRIBUTION PROJECT - BODC DATA
###################################################################
# R. Anderson, July/August 2017

rm(list=ls()) # clear environment

setwd("~/Oceanography/Sponge_Project")
library("oce","ggplot2")
library("stringr")
source("func.R")


region     <- "NwSea"
latLimits  <- c(64,71)
longLimits <- c(-10,15)

# REGION                    NAME
#
# Iceland-Greenland Sea     "Iceland_GS"
# Iceland                   "Iceland"
# Greenland Sea             "GrSea"
# Lofoten Basin             "Lofoten"
# Norwegian Sea             "NwSea"
# Schultz Massif            "Schultz"


#####
# READ IN DATA
#####

setwd("~/Oceanography/Sponge_Project/Data/")

geodia.hentscheli <- na.omit(read.csv("G_hentscheli_CARDENAS.csv"), 
                           stringsAsFactors = FALSE)

geodia.phlegraei <- na.omit(read.csv("G_phlegraei_CARDENAS.csv"), 
                   stringsAsFactors = FALSE)


geodiaH <- GeodiaExtract(geodia.hentscheli,c(64,71),c(-20,15))
geodiaP <- GeodiaExtract(geodia.phlegraei,c(64,71),c(-20,15))

geodiaH2 <- GeodiaExtract(geodiaH,c(71,74),c(-8,15))
geodiaP2 <- GeodiaExtract(geodiaP,c(71,74),c(-8,15))


# Get regional stations and import them from main data folder
#############################################################
files <- list.files(pattern = "*.lst") 
regional.stations <- scan(paste0("./",region,"/",region,"_Station_List.txt"), 
                          what="", sep="\n")

x = NULL
for (i in regional.stations){
  x[i] <- grep(i,files,value=TRUE)
}; 
file.copy(x, paste0("./",region)); rm(i,x,regional.stations)

setwd(paste0("~/Oceanography/Sponge_Project/Data/",region))
files <- list.files(pattern = "*.lst") 
##############################################################

#BODC data is irregular & must be filtered using grep, strsplit etc.
ctd <- lapply(files, function(i){
  x <- as.character(grep("Cycle",readLines(i)))  # finds first data entry
  y <- as.numeric(strsplit(x," "))
  skip <- y[2] - 1
  na.omit(read.table(i, skip = skip, header = TRUE, stringsAsFactors = FALSE, 
                     na.strings = "f"))
  
})
names(ctd) <- paste(files)

# Trim stations with < 400 data points from list
row_lt <- which(sapply(ctd, nrow) < 400)
if (length(row_lt) == 0) {  # necessary or it will cut all data on iterate
  print("File has >= 400 data points.")
} else {
  ctd <- ctd[-row_lt]
}; rm(row_lt)
files <- names(ctd)


# Fix row names
for (k in 1:length(ctd)) {
  row.names(ctd[[k]]) <- c(1:nrow(ctd[[k]]))
} ;rm(k)

# Remove letters from numerical data fields (actually replaces them with ''). 
# This should be generalised to search for all non-numerical entries but I can't
# get it working with a regular expression search in grep for some reason.
for (f in 1:length(ctd)) {
  ctd.i <- ctd[[f]]
  cols <- which(
    colSums(
      `dim<-`(grepl("M", as.matrix(ctd.i), fixed=TRUE), dim(ctd.i))
    ) > 0
  )
  if (length(cols) != 0) {
    print("Deleting characters in numerical fields...")
    for (k in cols) {
      ctd.i[[k]] <- lapply(ctd.i[[k]], gsub, pattern ='M', replacement='')
    }
    ctd[[f]] <- ctd.i
  }else {
    next
  }
} ;rm(f, cols)

# Standardise column names!!! And get rid of columns we don't need.
for (k in 1:length(ctd)) {
  x <- as.numeric(grep("TEMP",colnames(ctd[[k]])))
  y <- as.numeric(grep("SAL", colnames(ctd[[k]])))
  z <- as.numeric(grep("PRES", colnames(ctd[[k]])))
  colnames(ctd[[k]])[x] <- "TEMP"
  colnames(ctd[[k]])[y] <- "SALINITY"
  colnames(ctd[[k]])[z] <- "PRESSURE"
  ctd[[k]] <- data.frame(ctd[[k]][x], ctd[[k]][y], ctd[[k]][z])
};rm(x,y,z,k)

# Make all factors in data frame numeric (the above function converts some to lists)
for (i in 1:length(ctd)) {
  for (j in 1:length(ctd[[i]])) {
    ctd[[i]][[j]] <- as.numeric(ctd[[i]][[j]]) 
  }
}

##################################
# Potential Temperature Conversion
##################################

# Test for presence of pot.temp & convert temp to pot.temp if not found
# using 4th-order Runge-Kutta integration. Calls functions 'theta' and 'atg' 
# from 'func.R'.

# Some of the BODC data contains non-numeric entries in numeric-only fields;
# sequence below coerces non-numeric values to 'NA' (with warnings).
# This is generally bad as it removes useful data; non-numeric entries should 
# be removed earlier with grep if possible.

for (k in 1:length(ctd)) {
  
  # Fix Station number
  ctd.i <- ctd[[k]]
  if("POTMCV01" %in% colnames(ctd.i))
  {
    cat("All OK.\n");
    
  } else {
    cat("No pot.temp field found. Converting to pot.temp...\n");
    pressure <- as.numeric(ctd.i[,grep("PRES",colnames(ctd.i))])
    temp     <- as.numeric(ctd.i[,grep("TEMP",colnames(ctd.i))])
    sal      <- as.numeric(ctd.i[,grep("SAL",colnames(ctd.i))])
    
    for (j in c(1:length(sal))) {
      # reference pressure = 10 dbar
      ctd.i[j,"POTMCV01"] <- Theta(pressure[j], temp[j], sal[j], 10)  
    }
    
    ctd[[k]] <- ctd.i
    rm(pressure,temp,sal)
    
  }
  
};rm(j,k)


# Pressure to depth conversion
##############################

# Call 'PressToDepth' from func.R to first convert BODC 
# coords to decimal latitude, then convert pressure data to depth. 
for (f in files) {
  ctd.i <- ctd[[f]]
  ctd.i[,"DEPTH"] <- PressToDepth(f)
  ctd[[f]] <- ctd.i
  rm(ctd.i)
}; cat("Converted pressure to depth."); rm(f)


# Iterate through files and extracts BODC co-ordinate data using
# regular expression search.

bodc.coords <- vector() # placeholder for BODC data
for(f in files) {
  text = read.table(f,sep = "\t", stringsAsFactors =  FALSE)
  bodc.coords[f] <- str_match(text,"\\d+\\w+\\d+.\\d\\w+\\d+\\w+\\d+.\\d+\\w+")
  rm(text)
}; rm(f)

# Remove top 200m, all stations
for (i in 1:length(ctd)) {
  ctd.i <- ctd[[i]]
  x <- min(grep("^2\\d\\d.\\d+",ctd.i$DEPTH)) - 1
  ctd.i[1:x,] <- NA
  ctd[[i]] <- na.omit(ctd.i)
  rm(ctd.i)
};rm(x,i); cat("Removed top 200m.")


# Average all stations
######################

entries <- vector()
for (k in 1:length(ctd)) {
  entries[k] <- nrow(ctd[[k]])
} 
entries <- sort(entries)

ctd.avg <- data.frame(TEMP = numeric(),
                      SAL  = numeric(),
                      stringsAsFactors = FALSE)
allTemp <- vector()
allSal  <- vector()

for (i in 1:length(ctd)) {
  if (i==1) {
    for (j in 1:entries[1]) {
      l = 1
      for (k in order(sapply(ctd,nrow))) {
        allTemp[l] <- ctd[[k]][j,grep("TEMP",names(ctd[[k]]))]
        allSal [l] <- ctd[[k]][j,grep("SAL",names(ctd[[k]]))]
        l = l + 1
      }
      avgTemp <- sum(allTemp) / length(ctd)
      avgSal  <- sum(allSal) /  length(ctd)
      ctd.avg[j,1] <- avgTemp 
      ctd.avg[j,2] <- avgSal
    }
  } else {
    for (j in (entries[i-1]:entries[i]) ) {
      allTemp <- NULL
      allSal  <- NULL
      l = 1
      for (k in order(sapply(ctd,nrow))) {
        allTemp[l] <- ctd[[k]][j,grep("TEMP",names(ctd[[k]]))]
        allSal [l] <- ctd[[k]][j,grep("SAL",names(ctd[[k]]))]
        l = l + 1
      }
      avgTemp <- sum(na.omit(allTemp)) / length(na.omit(allTemp))
      avgSal  <- sum(na.omit(allSal)) /  length(na.omit(allTemp))
      ctd.avg[j,1] <- avgTemp 
      ctd.avg[j,2] <- avgSal
    }
  }
};rm(i,j,k,l,entries,allSal,allTemp,avgSal,avgTemp) 
cat("Average of all station data created as 'ctd.avg'."); 


# Check averaging script using random sample.
x <- numeric()
y <- numeric()
z <- sample(1:1000,1)
for (i in 1:length(ctd)) {
  x[i] <- ctd[[i]][z,grep("TEMP",names(ctd[[i]]))]
}; y <- sum(na.omit(x))
if (ctd.avg[z,grep("TEMP",names(ctd.avg))] == y/length(na.omit(x))) {
  cat("Averaging process tested OK.")
} else {
  cat("Averages do not match. Check code for errors.")
};rm(x,y)


geodiaH.temp <- geodiaH1[[grep("TEMP", names(geodiaH),ignore.case = TRUE)]]
geodiaH.sal <- geodiaH1[[grep("SAL", names(geodiaH), ignore.case = TRUE)]]

# geodiaP.temp <- geodia.parv[[grep("TEMP", names(geodia),ignore.case = TRUE)]]
# geodiaP.sal <- geodia.parv[[grep("SAL", names(geodia), ignore.case = TRUE)]]

lat <- NULL
for (f in files) {
  lat[f] = Lats(f)
};rm(f)