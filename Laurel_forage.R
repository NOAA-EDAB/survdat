#survey_template.r
#template to get from survdat.Rdata to swept area biomass estimates
#8/14
#SML

#User parameters
window <- F
if(window == T){
  data.dir <- "L:\\EcoAP\\SurvanLike\\Forage\\"
  out.dir  <- "L:\\EcoAP\\SurvanLike\\Forage\\"
  r.dir    <- "L:\\EcoAP\\SurvanLike\\Forage\\"
  gis.dir  <- "L:\\EcoAP\\SurvanLike\\Forage\\"
  }
if(window == F){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/misc/"
  r.dir    <- "slucey/Rworkspace/RSurvey/"
  gis.dir  <- "slucey/Rworkspace/GIS_files"
  uid <- 'slucey'
  cat('Oracle Password:')
  pwd <- readLines(n=1)
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(rgdal)

#-------------------------------------------------------------------------------
#User created functions
source(paste(r.dir, "Poststrat.r", sep = ''))
source(paste(r.dir, "getarea.r",   sep = ''))
source(paste(r.dir, "Prestrat.r",  sep = ''))
source(paste(r.dir, "Stratmean.r", sep = ''))
source(paste(r.dir, "Sweptarea.r", sep = ''))
    
#-------------------------------------------------------------------------------
#Run Survdat.r to generate Survdat.RData
#Load Survdat.RData
load(paste(data.dir, "Survdat.RData", sep = ''))

#Step 1 - change stratification
#Grab shapefile for new strata
#EPU <- readOGR(gis.dir, 'EPU')

#Use only station data
#setkey(survdat, CRUISE6, STRATUM, STATION)
#stations <- unique(survdat)
#stations <- stations[, list(CRUISE6, STRATUM, STATION, LAT, LON)]

#Poststratify
#stations.epu <- poststrat(stations, stratum = 'EPU', strata.col = 'EPU')
#setnames(stations.epu, "newstrata", "EPU")
#stations.epu[, c('LON', 'LAT') := NULL]

#Merge back to survdat
#survdat.epu <- merge(survdat, stations.epu, by = key(survdat)) 
#survdat.shelf <- merge(survdat, stations, by = key(survdat)) 

#Step 2 - Segregate by season
#fall <- survdat.epu[SEASON == 'FALL', ]
fall <- survdat[SEASON == 'FALL', ]

#Step 3 - Generate stratum area table
#epu.area <- getarea(EPU, strat.col = 'EPU')
area.shelf <- read.csv("Stratum_areas.csv")

#If using survey:
# #Add in connection to oracle to get species areas
# if(window == T)channel <- odbcDriverConnect()
# if(window == F)channel <- odbcConnect('sole', uid, pwd)
# 
# stratum <- as.data.table(sqlQuery(channel, "select STRATUM, STRATUM_AREA 
#                                             from SVMSTRATA"))
# odbcClose(channel)
# 
# #Convert from nm to km
# stratum[, STRATUM_AREA := STRATUM_AREA * 3.429904]


#Step 4 - Preprocess the data for stratified means
#fall.pre <- prestrat(fall, epu.area, strat.col = 'EPU', area.col = 'Area')
fall.pre <- prestrat(fall, area.shelf, strat.col = 'STRATUM', area.col = 'Area')

#Step 5 - reduce or aggregate species
fall.pre.forage <- fall.pre[SVSPP %in% c(31:37, 43, 44, 46, 47, 113, 121, 131:133, 181, 428, 429, 502, 503, 734, 865), Group := "Forage"]
setkey(fall.pre.forage, CRUISE6, STRATUM, STATION, Group)
fall.pre.forage[, biomass.new   := sum(BIOMASS),   by = key(fall.pre.forage)]
fall.pre.forage[, abundance.new := sum(ABUNDANCE), by = key(fall.pre.forage)]
fall.pre.forage <- unique(fall.pre.forage)
fall.pre.forage[, c('BIOMASS', 'ABUNDANCE') := NULL]
setnames(fall.pre.forage, c('biomass.new', 'abundance.new'), c('BIOMASS', 'ABUNDANCE'))

#Step 6 - Calculate stratified mean
strat.mean.forage <- stratmean(fall.pre.forage, group.col = 'Group', strat.col = 'STRATUM')

#Step 7 - Calculate swept area
gadid.totbiomass <- sweptarea(fall.pre.gadids, strat.mean.gadid, strat.col = 'EPU', area.col = 'Area', group.col = 'SVSPP')

#Output - either RData or csv
save(     gadid.totbiomass, file = paste(out.dir, "Gadid_Fall_Biomass.RData", sep = ''))
write.csv(gadid.totbiomass, file = paste(out.dir, "Gadid_Fall_Biomass.csv",   sep = ''), row.names = F)
