#New Survey Template for use with Survdat package
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
    r.dir    <- "L:\\Rworkspace\\RSurvey"
    data.dir <- "L:\\EcoAP\\Data\\survey"
    gis.dir  <- "L:\\Rworkspace\\GIS_files"
    out.dir  <- "L:\\EcoAP\\Data\\survey"
    memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
    r.dir    <- "/home/slucey/slucey/Rworkspace/RSurvey"
    data.dir <- "/home/slucey/slucey/EcoAP/Data/survey"
    gis.dir  <- "/home/slucey/slucey/Rworkspace/GIS_files"
    out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(rgdal); library(Survdat)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Grab survdat.r
load(file.path(data.dir, 'Survdat.RData'))

#Grab strata
strata <- readOGR(gis.dir, 'EPU_extended')

#Generate area table
strat.area <- getarea(strata, 'EPU')

#Post stratify data if necessary
survdat.epu <- poststrat(survdat, strata)
setnames(survdat.epu, 'newstrata', 'EPU')

#Subset by season/ strata set
fall   <- survdat.epu[SEASON == 'FALL', ]
spring <- survdat.epu[SEASON == 'SPRING']

#Calculate mean/variance per EPU
setkey(fall, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX)
fall <- unique(fall)

#Run stratification prep
fall.prep <- stratprep(fall,   strat.area, strat.col = 'EPU', area.col = 'Area')
sp.prep   <- stratprep(spring, strat.area, strat.col = 'EPU', area.col = 'Area')

#Calculate means
setkey(fall.prep, YEAR, EPU, SVSPP, CATCHSEX)
fall.prep[, biomass.sum := sum(BIOMASS,   na.rm = T), by = key(fall.prep)]
fall.prep[, abund.sum   := sum(ABUNDANCE, na.rm = T), by = key(fall.prep)]
fall.prep[, biomass.tow := biomass.sum / ntows]
fall.prep[, abund.tow   := abund.sum   / ntows]
fall.prep[, biomass.var := (zero.var + sum(var)) / (ntows - 1), by = key(fall.prep)]

#Calculate variance
#Calculate zero tows
fall.prep[, npos := length(unique(paste(STATION, STRATUM))), by = key(fall.prep)]
fall.prep[, nneg := ntows - npos]

#Biomass Var
fall.prep[, zero.var    := nneg * (0 - biomass.tow) ^ 2]
fall.prep[, var         := (BIOMASS - biomass.tow)  ^ 2]
fall.prep[, biomass.var := (zero.var + sum(var)) / (ntows - 1), 
          by = key(fall.prep)]

#Abundance Var
fall.prep[, zero.var.a := nneg * (0 - abund.tow)  ^ 2]
fall.prep[, var.a      := (ABUNDANCE - abund.tow) ^ 2]
fall.prep[, abund.var  := (zero.var.a + sum(var.a)) / (ntows - 1), 
          by = key(fall.prep)]

fall.mean <- unique(fall.prep[, list(YEAR, EPU, SVSPP, CATCHSEX, 
                                     biomass.tow, biomass.var,
                                     abund.tow,   abund.var)])



#Calculate total biomass/abundance estimates
#**Need to add pelagics**
load(file.path(data.dir, 'EMAX_groups.RData'))
q.table <- emax[SVSPP %in% esr.sp, ]

total.biomass <- sweptarea(GOM.prep, lob.mean, strat.col = 'STRATUM', 
                           area.col = 'Area')

#Output results either to a flat .csv file or .RData set
write.csv(total.biomass, file = paste(out.dir, 'Lobster_GoM.csv',   sep = ''), 
          row.names = F)
save(     total.biomass, file = paste(out.dir, 'Lobster_GOM.RData', sep = ''))
