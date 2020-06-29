#Prestrat.r
#Prepare data for Stratmean.r
#SML

prestrat <- function (survdat, areas, strat.col, area.col = 'area') {
  #survdat   <- data table created by Survdat.r
  #areas     <- data table with stratum and areas
  #strat.col <- column of survdat and areas with the strata names
  #area.col  <- column of areas with the area of the stratum in square kilometers
  #sp.col    <- column of survdat with the species codes of interest
  
  #-------------------------------------------------------------------------------
  #Required packages
  library(data.table)
  
  #-------------------------------------------------------------------------------
  #User created functions
  #count occurances
  count <- function(x){
    num <- rep(1, length(x))
    out <- sum(num)
    return(out)
    }
    
  #-------------------------------------------------------------------------------
  x <- copy(survdat)
  y <- copy(areas)
  
  setnames(x, strat.col, 'STRAT')
  setnames(y, c(strat.col, area.col), 
              c('STRAT',   'S.AREA'))
                         
  #Station data - remove catch/length
  setkey(x, CRUISE6, STRAT, STATION)
  stations <- unique(x)
  stations[, c(names(x)[which(!names(x) %in% c('YEAR', 'CRUISE6', 'STRAT', 'STATION'))]) := NULL]

  #count stations
  setkey(stations, YEAR, STRAT)
  stations[, ntows := count(STATION), by = key(stations)]

  #Merge stations and area
  stations <- merge(stations, y, by = 'STRAT', all.x = T)

  #Calculate stratum weight
  setkey(stations, 'YEAR', 'STRAT')
  strat.year <- unique(stations)
  strat.year[, c('CRUISE6', 'STATION', 'ntows') := NULL]
  strat.year[, W.h := S.AREA / sum(S.AREA, na.rm = T), by = YEAR]
  strat.year[is.na(W.h), W.h := 0]
  strat.year[, S.AREA := NULL]

  #Merge back
  stations <- merge(stations, strat.year, by = key(stations))

  #Merge catch with station data
  strat.survdat <- merge(x, stations, by = c('YEAR', 'CRUISE6', 'STRAT', 'STATION'))
  
  setnames(strat.survdat, c('STRAT',   'S.AREA'),
                          c(strat.col, area.col))
  
  return(strat.survdat)
  }