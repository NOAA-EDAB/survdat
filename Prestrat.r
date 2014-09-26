#Prestrat.r v.1.1
#Prepare data for Stratmean.r
#6/14
#V1.1 - Added the ability to use different groups than svspp and biomass
#SML

prestrat <- function (survdat, areas, strat.col, area.col = 'area', sp.col = 'SVSPP') {
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
  count<-function(x){
    num<-rep(1,length(x))
    out<-sum(num)
    return(out)
    }
    
  #-------------------------------------------------------------------------------
  x <- copy(survdat)
  y <- copy(areas)
  
  setnames(x, c(strat.col, sp.col),
              c('STRAT',   'SP'))
  setnames(y, c(strat.col, area.col), 
              c('STRAT', 'S.AREA'))
              
  len.var <- c('LENGTH', 'NUMLEN')
  sta.var <- c('SVVESSEL', 'YEAR', 'SEASON', 'LAT', 'LON', 
               'DEPTH', 'SURFTEMP', 'SURFSALIN', 'BOTTEMP', 'BOTSALIN')
  spp.var <- c('SP', 'CATCHSEX', 'ABUNDANCE', 'BIOMASS')

  #Catch data - remove length data
  setkey(x,
         CRUISE6,
         STRAT,
         STATION,
         SP,
         CATCHSEX)
  
  catch <- unique(x)
  catch[, c(len.var, sta.var) := NULL]

  #Merge sexed species
  setkey(catch,
         CRUISE6,
         STRAT,
         STATION,
         SP)

  biomass   <- catch[, sum(BIOMASS),      by = key(catch)]
  abundance <- catch[, sum(ABUNDANCE), by = key(catch)]
  setnames(biomass,   "V1", "BIOMASS")
  setnames(abundance, "V1", "ABUNDANCE")
  catch <- merge(biomass, abundance, by = key(catch))
               
  #Station data - remove catch/length
  setkey(x, CRUISE6, STRAT, STATION)
  stations <- unique(x)
  stations[, c(len.var, spp.var) := NULL]

  #count stations
  setkey(stations, YEAR, STRAT)
  stations[, ntows := count(STATION), by = key(stations)]

  #Merge stations and area
  stations <- merge(stations, y, by = 'STRAT')

  #Calculate stratum weight
  setkey(stations, 'YEAR', 'STRAT')
  strat.year <- unique(stations)
  strat.year[, which(!names(strat.year) %in% c('YEAR', 'STRAT', 'S.AREA')) := NULL]
  strat.year[, W.h := S.AREA / sum(S.AREA, na.rm = T), by = YEAR]
  strat.year[is.na(W.h), W.h := 0]
  strat.year[, S.AREA := NULL]

  #Merge back
  stations <- merge(stations, strat.year, by = key(stations))

  #Merge catch with station data
  strat.survdat <- merge(stations, catch, by = c('CRUISE6', 'STRAT', 'STATION'))
  
  setnames(strat.survdat, c('SP',   'STRAT',   'S.AREA'),
                          c(sp.col, strat.col, area.col))
  
  return(strat.survdat)
  }