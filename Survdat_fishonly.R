#User parameters
if(Sys.info()['sysname']=="Windows"){
    r.dir    <- "L:\\Rworkspace\\RSurvey\\"
    data.dir <- "L:\\EcoAP\\Data\\survey\\"
    gis.dir  <- "L:\\Rworkspace\\GIS_files"
    out.dir  <- "L:\\EcoAP\\Data\\survey\\"
    memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
    r.dir    <- "/home/slucey/slucey/Rworkspace/RSurvey/"
    data.dir <- "/home/slucey/slucey/EcoAP/Data/survey/"
    gis.dir  <- "/home/slucey/slucey/Rworkspace/GIS_files"
    out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey/"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(rgdal); library(Survdat)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Grab survdat.r
load(paste(data.dir, 'Survdat.RData', sep = ''))

#Grab svspp codes
svspp <- as.data.table(read.csv(paste(data.dir, 'SVSPP.csv', sep = '')))

fish <- merge(survdat, svspp[FISH == 'Y', ], by = 'SVSPP')



