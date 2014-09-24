#Generate files for maps

#Required packages
library(data.table)

#Set your directories
data.dir <- "L:\\EcoAP\\Data\\Survey\\"
out.dir  <- "L:\\EcoAP\\misc\\"

#Load Survdat_KK.RData
load(paste(data.dir, "Survdat_KK.RData", sep = ''))

#Generate station view
setkey(survdat, CRUISE6, STRATUM, STATION)
station.view <- unique(survdat)
station.view[, c('SVSPP', 'COMNAME', 'CATCHSEX', 'BIOMASS', 'ABUNDANCE', 'LENGTH', 'EXPNUMLEN') := NULL]
write.csv(station.view, file = paste(out.dir, "stationview.csv", sep = ''), row.names = F)

#Generate catchview
setkey(survdat, CRUISE6, STRATUM, STATION, SVSPP)
catch.view <- unique(survdat)
catch.view <- catch.view[, list(CRUISE6, STRATUM, TOW, STATION, SVSPP, COMNAME, CATCHSEX, BIOMASS, ABUNDANCE)]
#Pick your species
catch.view.104 <- catch.view[SVSPP == 104, ]
write.csv(catch.view.104, file = paste(out.dir, "catchview_104.csv", sep = ''), row.names = F)

#Generate lengthview
length.view <- survdat[, list(CRUISE6, STRATUM, TOW, STATION, SVSPP, COMNAME, CATCHSEX, LENGTH, EXPNUMLEN)]
#Pick your species
length.view.104 <- length.view[SVSPP == 104, ]
write.csv(length.view.104, file = paste(out.dir, "lengthview_104.csv", sep = ''), row.names = F)
