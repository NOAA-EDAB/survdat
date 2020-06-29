#Curtis_request
#SML

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
load(paste(data.dir, 'Survdat_raw.RData', sep = ''))

thorny <- survdat[SVSPP == 28 & YEAR %in% 2013:2015, ]

write.csv(thorny, file = paste(out.dir, 'Curtis_thorny_skates_13_15.csv', sep = ''), row.names = F)

load(paste(data.dir, 'SurvdatBio.RData', sep = ''))

thorny.bio <- survdat.bio[SVSPP == 28 & YEAR %in% 2013:2015, ]

write.csv(thorny.bio, file = paste(out.dir, 'Curtis_thorny_skates_bio_13_15.csv', sep = ''), row.names = F)





