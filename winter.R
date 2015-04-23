#winter.R
#extract winter survey
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey\\"
  out.dir  <- "L:\\EcoAP\\Data\\survey\\"
}

if(Sys.info()['sysname']=="Linux"){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/Data/survey/"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load Survdat.RData
load(paste(data.dir, "Survdat_allseason.RData", sep = ''))

survdat.winter <- survdat[SEASON == 'WINTER', ]

save(survdat.winter, file = paste(out.dir, "Survdat_winter.RData", sep=''))
