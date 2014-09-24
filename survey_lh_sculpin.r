#survey_lh_sculpin.r
#Longhorn sculpin survey index
#8/14
#SML

#User parameters
window <- F
if(window == T){
  data.dir <- "L:\\EcoAP\\Data\\survey\\"
  out.dir  <- "L:\\EcoAP\\misc\\"
  r.dir    <- "L:\\Rworkspace\\Survey\\"
  gis.dir  <- "L:\\Rworkspace\\GIS_files"
}
if(window == F){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/misc/"
  r.dir    <- "slucey/Rworkspace/Survey/"
  gis.dir  <- "slucey/Rworkspace/GIS_files"
  uid <- 'slucey'
  pwd <- '' #Need to enter actual password but do not save
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(rgdal); library(RODBC)

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
#NA

#Step 2 - Segregate by season
fall <- survdat[SEASON == 'FALL', ]

#Step 3 - Generate stratum area table
channel <- odbcConnect('sole', uid, pwd)
stratum <- as.data.table(sqlQuery(channel, "select STRATUM, STRATUM_AREA 
                                            from SVMSTRATA"))
odbcClose(channel)

#Convert from nm^2 to km^2
stratum[, STRATUM_AREA := STRATUM_AREA * 3.429904]
stratum[is.na(STRATUM_AREA), STRATUM_AREA := 0]

#Step 4 - Preprocess the data for stratified means
fall.pre <- prestrat(fall, stratum, strat.col = 'STRATUM', area.col = 'STRATUM_AREA')

#Step 5 - reduce or aggregate species
fall.pre.lh <- fall.pre[SVSPP == 163, ]

#Step 6 - Calculate stratified mean
lh.index <- stratmean(fall.pre.lh, group.col = 'SVSPP', strat.col = 'STRATUM')

#Output - either RData or csv
#save(     lh.index, file = paste(out.dir, "Longhorn_sculpin_index.RData", sep = ''))
write.csv(lh.index, file = paste(out.dir, "Longhorn_sculpin_index.csv",   sep = ''), row.names = F)
