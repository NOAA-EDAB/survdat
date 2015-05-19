if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/EcoAP/Data/survey/"
  out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey/"
  gis.dir  <- "/home/slucey/slucey/Rworkspace/GIS_files"
}

# library(devtools)
# devtools::install_github('slucey/RSurvey/Survdat', 
#                          auth_token = 'd95526d2fb3f6e34f9c8481b1740e0033ac1d623')

library(Survdat); library(data.table); library(rgdal)

load(paste(data.dir, 'Survdat.RData', sep = ''))

strata <- readOGR(gis.dir, 'strata')

strat.area <- getarea(strata, 'STRATA')
setnames(strat.area, 'STRATA', 'STRATUM')

fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in% c(1260:1300, 1340, 1351, 1360:1400,
                                                      3590:3610, 3640:3660), ]

GOM.prep <- stratprep(fall.GOM, strat.area, strat.col = 'STRATUM', area.col = 'Area')

lob.mean <- stratmean(GOM.prep, groups = 301, group.col = 'SVSPP', strat.col = 'STRATUM')

write.csv(lob.mean, file = paste(out.dir, 'Lobster_GoM_fall.csv', sep = ''), row.names = F)
