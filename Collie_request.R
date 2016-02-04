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
#Fall
#Remove lengths
setkey(fall, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX)
fall <- unique(fall)

#Run stratification prep
fall.prep <- stratprep(fall, strat.area, strat.col = 'EPU', area.col = 'Area')

#Calculate means
setkey(fall.prep, YEAR, EPU, SVSPP, CATCHSEX)
fall.prep[, biomass.sum := sum(BIOMASS,   na.rm = T), by = key(fall.prep)]
fall.prep[, abund.sum   := sum(ABUNDANCE, na.rm = T), by = key(fall.prep)]
fall.prep[, biomass.tow := biomass.sum / ntows]
fall.prep[, abund.tow   := abund.sum   / ntows]

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

#Spring
#Fall
setkey(spring, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX)
spring <- unique(spring)

#Run stratification prep
spring.prep <- stratprep(spring, strat.area, strat.col = 'EPU', area.col = 'Area')

#Calculate means
setkey(spring.prep, YEAR, EPU, SVSPP, CATCHSEX)
spring.prep[, biomass.sum := sum(BIOMASS,   na.rm = T), by = key(spring.prep)]
spring.prep[, abund.sum   := sum(ABUNDANCE, na.rm = T), by = key(spring.prep)]
spring.prep[, biomass.tow := biomass.sum / ntows]
spring.prep[, abund.tow   := abund.sum   / ntows]

#Calculate variance
#Calculate zero tows
spring.prep[, npos := length(unique(paste(STATION, STRATUM))), by = key(spring.prep)]
spring.prep[, nneg := ntows - npos]

#Biomass Var
spring.prep[, zero.var    := nneg * (0 - biomass.tow) ^ 2]
spring.prep[, var         := (BIOMASS - biomass.tow)  ^ 2]
spring.prep[, biomass.var := (zero.var + sum(var)) / (ntows - 1), 
          by = key(spring.prep)]

#Abundance Var
spring.prep[, zero.var.a := nneg * (0 - abund.tow)  ^ 2]
spring.prep[, var.a      := (ABUNDANCE - abund.tow) ^ 2]
spring.prep[, abund.var  := (zero.var.a + sum(var.a)) / (ntows - 1), 
          by = key(spring.prep)]

spring.mean <- unique(spring.prep[, list(YEAR, EPU, SVSPP, CATCHSEX, 
                                     biomass.tow, biomass.var,
                                     abund.tow,   abund.var)])

#Calculate total biomass/abundance estimates
#Load catchability from EMAX
load(file.path(data.dir, 'EMAX_groups.RData'))
q.table.fall   <- emax[, list(SVSPP, Fall.q)]
q.table.spring <- emax[, list(SVSPP, Spring.q)]
setnames(q.table.fall,   'Fall.q',   'q')
setnames(q.table.spring, 'Spring.q', 'q')

#Calculate Areas
strat.area[, a := 0.0384] #Standard Albatross tow
strat.area[, A.a := Area / a]

#Expand mean per tow by swept area
#Fall
fall.mean <- merge(fall.mean, q.table.fall, all.x = T, by = 'SVSPP')
fall.mean <- merge(fall.mean, strat.area, by = 'EPU')
fall.mean[, A.a.q := A.a / q]

fall.mean[, Total.biomass := biomass.tow * A.a.q]
fall.mean[, Tot.bio.var   := biomass.var * A.a.q ^ 2]

fall.mean[, Total.abundance := round(abund.tow * A.a.q)]
fall.mean[, Tot.abund.var   := abund.var * A.a.q ^ 2]

fall <- fall.mean[, c('q', 'Area', 'a', 'A.a', 'A.a.q') := NULL]

#Spring
spring.mean <- merge(spring.mean, q.table.spring, all.x = T, by = 'SVSPP')
spring.mean <- merge(spring.mean, strat.area, by = 'EPU')
spring.mean[, A.a.q := A.a / q]

spring.mean[, Total.biomass := biomass.tow * A.a.q]
spring.mean[, Tot.bio.var   := biomass.var * A.a.q ^ 2]

spring.mean[, Total.abundance := round(abund.tow * A.a.q)]
spring.mean[, Tot.abund.var   := abund.var * A.a.q ^ 2]

spring <- spring.mean[, c('q', 'Area', 'a', 'A.a', 'A.a.q') := NULL]

#Output results either to a flat .csv file or .RData set
write.csv(fall, file = file.path(out.dir, 'NEFSC_Fall_Survey.csv'), row.names = F)
save(     fall, file = file.path(out.dir, 'NEFSC_Fall_Survey.RData'))

write.csv(spring, file = file.path(out.dir, 'NEFSC_Spring_Survey.csv'), 
          row.names = F)
save(     spring, file = file.path(out.dir, 'NEFSC_Spring_Survey.RData'))

#Commercial Data
data.dir <- '/home/slucey/slucey/EcoAP/Data/Commercial'
out.dir <- data.dir

load(file.path(data.dir, 'comland_meatwt.RData'))

#Condense data to year/EPU
setkey(comland, YEAR, EPU, NESPP3)
landings.epu <- comland[, sum(SPPLIVMT), by = key(comland)]
setnames(landings.epu, 'V1', 'SPPLIVMT')

write.csv(landings.epu, file = file.path(out.dir, 'Commercial_Landings_by_EPU.csv'),
          row.names = F)
save(landings.epu, file = file.path(out.dir, 'Commercial_Landings_by_EPU.RData'))


