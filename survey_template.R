#New Survey Template for use with Survdat package
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
strata <- readOGR(gis.dir, 'strata')

#Generate area table
strat.area <- getarea(strata, 'STRATA')
setnames(strat.area, 'STRATA', 'STRATUM')

#Post stratify data if necessary
#survdat.epu <- poststrat(survdat, strata)
#setnames(survdat.epu, 'newstrata', 'EPU')

#Subset by season/ strata set
fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in% c(1220, 1240, 1260:1290, 
                                                      1360:1400), ]

#Run stratification prep
GOM.prep <- stratprep(fall.GOM, strat.area, strat.col = 'STRATUM', 
                      area.col = 'Area')

#Calculate stratified means - can do multiple species at once (i.e. groups = 
#c(72, 73, 75))
#You can also aggregate species here
#GOM.prep[SVSPP %in% 72:76, Group := 'Gadid']
#then use groups = 'Gadid' and group.col = 'Group' in stratmean
#Note: The function will merge aggregated groups for you
lob.mean <- stratmean(GOM.prep, groups = 301, group.col = 'SVSPP', 
                      strat.col = 'STRATUM')

#Calculate total biomass/abundance estimates
total.biomass <- sweptarea(GOM.prep, lob.mean, strat.col = 'STRATUM', 
                           area.col = 'Area')

#Output results either to a flat .csv file or .RData set
write.csv(total.biomass, file = paste(out.dir, 'Lobster_GoM.csv',   sep = ''), 
          row.names = F)
save(     total.biomass, file = paste(out.dir, 'Lobster_GOM.RData', sep = ''))
