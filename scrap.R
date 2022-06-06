processFile = function(test.txt) {
  con = file(test.txt, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

for(m in bodc.coords.df) {
  print(m)
}  

#splice <- ctd[,2:4]
#ctd$CPHLPR01 <- NA




##
# Method to read in multiple .lst files
##

setwd("\\\\fs-home-n/home-014/ossa25/My Documents/ProjectData/")
files <- list.files(pattern = "*.lst")



#### works for single file with known parameters
test <- na.omit(read.table("b0671369.lst", skip = 39, header = TRUE,
                           stringsAsFactors = FALSE, na.strings = "f"))

#### * Approach 1 
dat <- NULL
for (f in files) {
  dat <- na.omit(read.table(f, skip = 39, header = TRUE,
             stringsAsFactors = FALSE, na.strings = "f"))
};

#### * Approach 2: doesn't work as data files are irregular
df.list <- lapply(files, function(i){
  na.omit(read.table(i, skip = 39, header=TRUE, stringsAsFactors = FALSE, 
          na.strings = "f"))
})

### * Approach 3: use grep to get location of data headers - works

#test <- readLines("b1026080.lst")
x <- as.character(grep("Cycle",readLines("b1026080.lst")))
y <- as.numeric(strsplit(x," "))
skip <- y[2]

test <- na.omit(read.table("b1026080.lst", skip = skip-1, header = TRUE,
                           stringsAsFactors = FALSE, na.strings = "f"))

### * Loop approach 3

files <- list.files(pattern = "*.lst")
dat <- NULL
for (f in files){
  x <- as.character(grep("Cycle",readLines(f)))
  y <- as.numeric(strsplit(x," "))
  skip <- y[2]
  dat[f] <- na.omit(read.table(f, skip = skip-1, header=TRUE, stringsAsFactors = FALSE, 
                     na.strings = "f"))
}  # doesn't work


### * Loop v.2  ## WORKS! ###
files <- list.files(pattern = "*.lst")

df.list <- lapply(files, function(i){
  x <- as.character(grep("Cycle",readLines(i)))
  y <- as.numeric(strsplit(x," "))
  skip <- y[2]
  na.omit(read.table(i, skip = skip-1, header=TRUE, stringsAsFactors = FALSE, 
                     na.strings = "f"))
})









###
#NAMING AND EXTRACTING STATIONS ON LIST 
###

stations <- c("b0671369.lst", "b1026080.lst", "b1035403.lst", "b1136942.lst", 
              "b1137110.lst")  # not necessary


b0671369.lst <- data.frame(df.list[1]) # works for single station

### * Attempt 1:  doesn't work
n=1;
for (i in stations){
  i <- data.frame(df.list[n]) 
  n=n+1
}
 
### * Attempt 2: works

names(df.list) <- paste(files) 

list2env(df.list, envir = .GlobalEnv)



##########################################################################
# TEMPERATURE TO POT.TEMP CONVERSION FUNCTION
##########################################################################

source("\\\\fs-home-n/home-014/ossa25/My Documents/ProjectData/func.R")
theta(10000,40,40,0)


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
  
  ## MUST END LINES WITH OPERATOR FOR CALCULATION TO CONTINUE! ###
  
  a = (((-2.1687E-13*t + 1.8676E-11)*t - 4.6206E-10)*p + 
         (( 2.7759E-10*t - 1.1351E-08)*s + ((-5.4481E-12*t + 8.7330E-10)*t - 
                                              6.7795E-08)*t + 1.8741E-06))*p + (-4.2393E-07*t + 1.8932E-05)*s + 
    (( 6.6228E-09*t - 6.8360E-07)*t + 8.5258E-05)*t + 3.5803E-04 ;
  
  
  return(a)
}



# TESTS FOR PRESENCE OF POT. TEMP

for (k in 1:length(files)) {
  # Fix Station number
  ctd.i <- ctd[[k]]
  if("POTMCV01" %in% colnames(ctd.i))
  {
    cat("Yep, it's in there!\n");
  } else {
    cat("Nope.\n");
  }
} 


#########################################
# POTENTIAL TEMPERATURE CODE
#########################################

for (k in 1:length(ctd)) {
  
  # Fix Station number
  ctd.i <- ctd[[k]]
  if("POTMCV01" %in% colnames(ctd.i))
  {
    cat("All OK.\n");
  } else {
    cat("No pot.temp field found. Converting to pot.temp...\n");
    pressure <- ctd.i[,grep("PRES",colnames(ctd.i))]
    temp     <- ctd.i[,grep("TEMP",colnames(ctd.i))]
    sal      <- ctd.i[,grep("SAL",colnames(ctd.i))]
    #ctd.i[,"POTEMP"] <- 0  # placeholder
    
      for (j in c(1:length(sal))) {
       ctd.i[j,"POTMCV01"] <- theta(pressure[j], temp[j], sal[j], 10)  # reference pressure = 10 dbar
       }
    
    ctd[[k]] <- ctd.i
    rm(pressure,temp,sal)
    
    }
  rm(ctd.i)
}


list2env(ctd,.GlobalEnv)

#################
# TESTING POTEMP CONVERSION
#################
test <- data.frame(b1136978.lst[,c("PRESPR01","PSALST01","TEMPST01","POTMCV01")])
p <- test[,"PRESPR01"]
s <- test[,"PSALST01"]
t <- test[,"TEMPST01"]

for (j in c(1:length(t))) {
  test[j,"POTEMP_0BAR"] <- theta(p[j], t[j], s[j], 0)  # reference pressure = 10 dbar
}

### CONVERSION IS CORRECT WITH REF. PRESSURE = 1 BAR = 10 dBAR ###



################
# COORDINATE CONVERTION
###############

# Converts BODC data in Degrees-Decimal-Minutes to decimal degrees.
setwd("\\\\fs-home-n/home-014/ossa25/My Documents/ProjectData/Data")

bodc.coords <- vector() # to hold BODC data

# Iterates through files and extracts BODC co-ordinate data using
# regular expression search, then converts to data frame.
for(f in files) {
  text = read.table(f,sep = "\t", stringsAsFactors =  FALSE)
  bodc.coords[f] <- str_match(text,"\\d+\\w+\\d+.\\d\\w+\\d+\\w+\\d+.\\d+\\w+")
  rm(text)
}; df.bodc.coords <- data.frame(bodc.coords,stringsAsFactors = FALSE)

# string search to retrieve integer and fractional part of decimal latitude
regexp.int <- "..."  
regexp.frac <- "\\d+[.]\\d+"  
lat.int <- as.numeric(str_extract(bodc.coords, regexp.int))
lat.frac <- as.numeric(str_extract(bodc.coords, regexp.frac))
lat.frac <- lat.frac/60 

latitude = lat.int + lat.frac

df.bodc.coords[,"LATITUDE"] <- latitude




#########
# Pressure to depth conversion
#########

for (k in 1:length(files)) {
  # Fix Station number
  ctd.i <- ctd[[k]]
  
  pressure <- ctd.i[,grep("PRES",colnames(ctd.i))]
  
  waterDepth <- pressure*0L # create a matrix of zeroes to hold data
  g <- pressure*0L
  
  # From UNESCO Technical Papers in Marine Science No. 44
  for (i in c(1:length(pressure))){
    x <- sin(63.11533/(360/(2*pi)))^2
    g[i] <- 9.780318 * ( 1.0 + (( 5.2788e-3  + (2.36e-5  * x)) * x )) + (1.092e-6  * pressure[i] )
    waterDepth[i] <- ((((-1.82e-15  * pressure[i] + 2.279e-10 ) * pressure[i] - 2.2512e-5 ) * pressure[i] + 9.72659) * pressure[i]) / g[i]
  }
  #ctd$waterDepth <-
  
}

### Consider creating CTD object? ###


## practice ##
for (f in files){
  print(f)
  View(ctd[f])
}


for (f in files) {
  ctd.i <- ctd[[f]]
  ctd.i[,"DEPTH"] <- press2depth(f)
  ctd[[f]] <- ctd.i
  rm(ctd.i)
}



for (k in 1:length(ctd)) {
  
  # Fix Station number
  ctd.i <- ctd[[k]]
  potemp(f)
  ctd[[k]] <- ctd.i
  rm(ctd.i)
    
}


col = rgb(runif(3),runif(3),runif(3)) 



# search data frame variable names
x <- grep("SAL", names(ctd.i), value=TRUE)


cols <- which(
  colSums(
    `dim<-`(grepl("M", as.matrix(ctd.i), fixed=TRUE), dim(ctd.i))
  ) > 0
)

rows <- which(rowSums(as.matrix(ctd.i) == "M") > 0)


col <- grep("PRES",names(ctd.i), value = TRUE)


# list2env(ctd, envir = .GlobalEnv)  # extract listed data frames to environment



# # Deals with the mess left by people daft enough to put letters in numerical data fields
# for (f in files) {
#   #ctd.i <- ctd[[f]]
#   if (is.character(ctd[[f]]$PSALST01)) {
#   ctd[[f]]<- lapply(ctd[[f]], gsub, pattern='M', replacement='')
#   #ctd[[f]] <- ctd.i
#   } else {
#     next
#   }
# }

### 
# FINDING REGIONAL DATA AND DITCHING THE REST
###

grep(1746039,files, value = TRUE)  # an example

for (i in lofoten.stations){
  grep(i,files,value=TRUE)
}


# Check existence of directory and create if doesn't exist
dir.create("~/Oceanography/Sponge_Project/Data/Lofoten")

setwd(paste0("~/Oceanography/Sponge_Project/",var,"/GS"))


###############
# AVERAGING REGIONAL CTD PROFILES
###############

entries <- vector()
for (k in 1:length(ctd)) {
  entries[k] <- nrow(ctd[[k]])
} 
entries <- sort(entries)


testCTD <- ctd
  
newCtd <- data.frame(TEMP = numeric(),
                     SAL  = numeric(),
                     stringsAsFactors = FALSE)

#  works for first 'batch'
allTemp <- NULL
allSal  <- NULL
for (i in 1:entries[1]){
  for (j in 1:length(ctd)) {
    allTemp[j] <- ctd[[j]][i,grep("TEMP",names(ctd[[j]]))]
    allSal [j] <- ctd[[j]][i,grep("SAL",names(ctd[[j]]))]
  }
  avgTemp <- sum(allTemp) / length(ctd)
  avgSal  <- sum(allSal) /  length(ctd)
  newCtd[i,1] <- avgTemp 
  newCtd[i,2] <- avgSal
}



# main code, this one works
for (i in 1:length(ctd)) {
  if (i==1) {
    l = 1
    for (j in 1:entries[1]) {
      for (k in order(sapply(ctd,nrow))) {
        allTemp[l] <- ctd[[k]][j,grep("TEMP",names(ctd[[k]]))]
        allSal [l] <- ctd[[k]][j,grep("SAL",names(ctd[[k]]))]
        l = l + 1
      }
      avgTemp <- sum(allTemp) / length(ctd)
      avgSal  <- sum(allSal) /  length(ctd)
      newCtd[j,1] <- avgTemp 
      newCtd[j,2] <- avgSal
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
      newCtd[j,1] <- avgTemp 
      newCtd[j,2] <- avgSal
    }
  }
}

geodia.sal <- geodia$Salinity..ppm.
geodia.temp <- geodia$Temp....C.

sorted_dataframes_list <- lapply(ctd, function(df){
  df[order(df$enrichment),]
})




# try again
for (i in 1:length(ctd)) {
  if (i==1) {
    l = 1
    for (j in 1:entries[1]) {
      for (k in order(sapply(ctd,nrow))) {
        allTemp[l] <- ctd[[k]][j,grep("TEMP",names(ctd[[k]]))]
        allSal [l] <- ctd[[k]][j,grep("SAL",names(ctd[[k]]))]
        l = l + 1
      }
      avgTemp <- sum(allTemp) / length(ctd)
      avgSal  <- sum(allSal) /  length(ctd)
      newCtd[j,1] <- avgTemp 
      newCtd[j,2] <- avgSal
    }
  } else {
    for (j in (entries[i-1]:entries[i]) ) {
      l = 1
      for (k in order(sapply(ctd,nrow))) {
        allTemp[l] <- ctd[[k]][j,grep("TEMP",names(ctd[[k]]))]
        allSal [l] <- ctd[[k]][j,grep("SAL",names(ctd[[k]]))]
        l = l + 1
      }
      avgTemp <- sum(na.omit(allTemp)) / length(na.omit(allTemp))
      avgSal  <- sum(na.omit(allSal)) /  length(na.omit(allTemp))
      newCtd[j,1] <- avgTemp 
      newCtd[j,2] <- avgSal
    }
  }
}

  
# add temperature from each ctd profile at particular row entry then divide by number 
# of profiles to get avg for that row

allTemps <- NULL
for (k in 1:length(ctd)) {
  allTemps[k] <- ctd[[k]][102,grep("TEMP",names(ctd[[k]]))]
}
avg <- sum(allTemps) / length(ctd)


 salinity <- ctd.i$SALINITY
 test <- as.ctd("salinity","temperature","pressure")
 ctd.i <- ctdDecimate(test, p=1, method="boxcar")

 
 ######
 # EXTRACT REGIONAL GEODIA DATA
 ######
 
 GeodiaExtract <- function(geodia,latLimits,longLimits) {
   
   x <- data.frame(lapply(geodia, function(x) x <- NA))
   
   lowerLatLimit  <- latLimits[1]
   upperLatLimit  <- latLimits[2]
   lowerLongLimit <- longLimits[1]
   upperLongLimit <- longLimits[2]
   
   for (i in 1:length(geodia[[1]])) {
     if (is.na(geodia[i,grep("Lat",names(geodia),ignore.case == T)]) == TRUE) {
       next
     }else if (geodia[i,grep("Lat",names(geodia),ignore.case == T)] >= lowerLatLimit && 
               geodia[i,grep("Lat",names(geodia),ignore.case == T)] <= upperLatLimit &&
               geodia[i,grep("Lon",names(geodia),ignore.case == T)] >= lowerLongLimit && 
               geodia[i,grep("Lon",names(geodia),ignore.case == T)] <= upperLongLimit) 
     {
       x[i,] <- geodia[i,]
     }else { 
       next
     }
   }
   return(x)
 }
 
 
 
 
 x <- data.frame(lapply(geodia, function(x) x <- NA))
 
 lowerLongLimit <- -10
 upperLongLimit <- 15
 lowerLatLimit  <- 64
 upperLatLimit  <- 71
 
 for (i in 1:length(geodia[[1]])) {
   if (is.na(geodia[i,"Lat"]) == TRUE) {
     next
   }else if (geodia[i,"Lat"] >= lowerLatLimit && geodia[i,"Lat"] <= upperLatLimit &&
             geodia[i,"Lon"] >= lowerLongLimit && geodia[i,"Lon"] <= upperLongLimit) {
     x[i,] <- geodia[i,]
   }else { 
     next
   }
 }

