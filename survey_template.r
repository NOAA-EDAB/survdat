#survey_template.r
#template to get from survdat.Rdata to swept area biomass estimates
#8/14
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey\\"
  out.dir  <- "L:\\EcoAP\\misc\\"
  r.dir    <- "L:\\Rworkspace\\Survey\\"
  gis.dir  <- "L:\\Rworkspace\\GIS_files"
}

if(Sys.info()['sysname']=="Linux"){
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
EPU <- readOGR(gis.dir, 'EPU')

#Use only station data
setkey(survdat, CRUISE6, STRATUM, STATION)
stations <- unique(survdat)
stations <- stations[, list(CRUISE6, STRATUM, STATION, LAT, LON)]

#Poststratify
stations.epu <- poststrat(stations, stratum = 'EPU', strata.col = 'EPU')
setnames(stations.epu, "newstrata", "EPU")
stations.epu[, c('LON', 'LAT') := NULL]

#Merge back to survdat
survdat.epu <- merge(survdat, stations.epu, by = key(survdat)) 

#Step 2 - Generate stratum area table
epu.area <- getarea(EPU, strat.col = 'EPU')
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


#Step 3 - Segregate by season
fall <- survdat.epu[SEASON == 'FALL', ]

#Step 4 - Preprocess the data for stratified means
#Need to combine sexed species or generate a new species code - This is 
#no longer done as part of prestrat
fall.pre <- prestrat(fall, epu.area, strat.col = 'EPU', area.col = 'Area')

#Step 5 - reduce or aggregate species
fall.pre.gadids <- fall.pre[SVSPP %in% c(72:74, 76), ]
#could alternatively do - but make sure to aggregate the group
#fall.pre[SVSPP %in% c(72:74, 76), Group := 'Gadid"]
# setkey(fall.pre, CRUISE6, STRATUM, STATION, Group)
# fall.pre[, biomass.new   := sum(BIOMASS),   by = key(fall.pre)]
# fall.pre[, abundance.new := sum(ABUNDANCE), by = key(fall.pre)]
# fall.pre <- unique(fall.pre)
# fall.pre[, c('BIOMASS', 'ABUNDANCE') := NULL]
# setnames(fall.pre, c('biomass.new', 'abundance.new'), c('BIOMASS', 'ABUNDANCE'))
# fall.pre <- fall.pre[!is.na(Group), ]
#And run stratmean off group.col = 'Group' instead of 'SVSPP'

#Step 6 - Calculate stratified mean
#You can now use an aggregate biomass or abundance by specifying weight/number parameter
strat.mean.gadid <- stratmean(fall.pre.gadids, group.col = 'SVSPP', strat.col = 'EPU')

#Step 7 - Calculate swept area
gadid.totbiomass <- sweptarea(fall.pre.gadids, strat.mean.gadid, strat.col = 'EPU', area.col = 'Area', group.col = 'SVSPP')

#Output - either RData or csv
save(     gadid.totbiomass, file = paste(out.dir, "Gadid_Fall_Biomass.RData", sep = ''))
write.csv(gadid.totbiomass, file = paste(out.dir, "Gadid_Fall_Biomass.csv",   sep = ''), row.names = F)




