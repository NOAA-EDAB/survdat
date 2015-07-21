#Tam_relative_abundance.R
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
load(paste(data.dir, 'Survdat.RData', sep = ''))

#Grab strata
strata <- readOGR(gis.dir, 'EPU')

#Generate area table
strat.area <- getarea(strata, 'EPU')

#Post strat data
survdat.epu <- poststrat(survdat, strata)
setnames(survdat.epu, 'newstrata', 'EPU')

#Subset by season/ strata set
fall   <- survdat.epu[SEASON == 'FALL', ]
spring <- survdat.epu[SEASON == 'SPRING', ]

#Run stratification prep
fall.prep   <- stratprep(fall,   strat.area, strat.col = 'EPU', area.col = 'Area')
spring.prep <- stratprep(spring, strat.area, strat.col = 'EPU', area.col = 'Area')

#Calculate mean abundance per EPU
setkey(fall.prep, YEAR, SVSPP, EPU, STATION, STRATUM)
fall.prep2 <- unique(fall.prep)
setkey(fall.prep2, YEAR, SVSPP, EPU)
fall.mean   <- unique(fall.prep2[, sum(ABUNDANCE, na.rm = T) / ntows, by = key(fall.prep2)])
setnames(fall.mean, 'V1', 'n.tow')

setkey(spring.prep, YEAR, SVSPP, EPU, STATION, STRATUM)
spring.prep2 <- unique(spring.prep)
setkey(spring.prep2, YEAR, SVSPP, EPU)
spring.mean   <- unique(spring.prep2[, sum(ABUNDANCE, na.rm = T) / ntows, by = key(spring.prep2)])
setnames(spring.mean, 'V1', 'n.tow')

#Output results either to a flat .csv file or .RData set
write.csv(fall.mean,   file = paste(out.dir, 'Tam_relative_abundance_fall.csv',     sep = ''), row.names = F)
write.csv(spring.mean, file = paste(out.dir, 'Tam_relative_abundance_spring.csv',   sep = ''), row.names = F)

