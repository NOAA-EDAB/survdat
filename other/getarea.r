#getarea.r
#quick script to calculate stratum areas from a shapefile
#Area is in square kilometers
#6/14
#SML


getarea <- function(shape, strat.col){
  #shape     <- shape file
  #strat.col <- column of which to calculate the area

  #Required packages
  library(rgdal); library(data.table)

  #Get stratum areas
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  
  strat <- spTransform(shape, lcc)
  
  #Calculate areas
  names(strat@data)[which(names(strat@data) == strat.col)] <- "STRAT"
  strata.A <- data.table(STRAT = slot(strat, "data")$STRAT, Area = sapply(slot(strat, "polygons"), slot, "area")/1e6)
  
  setnames(strata.A, "STRAT", strat.col)
  
  return(strata.A)
  }





