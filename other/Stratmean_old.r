#Stratmean.r v.1.3
#Generate stratified means from survey data
#11/13
#v1.4 - fixed renaming of columns
#v1.3 - fixed SE of mean
#v1.2 - added SE
#SML

stratmean <- function (x, stratum = 'EPU', strata.col = 'EPU', x.strata.col = strata.col, groups = 'SVSPP') {
  #x <- data to stratify
  #stratum <- r object containing the shapefile
  #strata.col <- stratum@data column that contains strata designations
  #x.strata.col <- column of x that contains the strata designations
  #groups <- stratified mean based on these groups (i.e. svspp)

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
  #Get stratum areas
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  
  strat <- spTransform(get(stratum), lcc)
  
  #Calculate areas
  names(strat@data)[which(names(strat@data) == strata.col)] <- "STRAT"
  strata.A <- data.table(STRAT = slot(strat, "data")$STRAT, Area = sapply(slot(strat, "polygons"), slot, "area")/1e6)

  setnames(x,
          c(x.strata.col, groups),
          c('STRAT', 'GROUP'))

  #Get station x
  setkey(x,
        CRUISE6,
        STATION,
        STRAT)
          
  stations <- unique(x)

  setkey(stations,
        STRAT,
        YEAR)

  station.sums <- stations[, count(STATION), by = key(stations)]

  setnames(station.sums,
            "V1",
            "ntows")

  #Determine Strata weight
  station.sums <- merge(station.sums, strata.A, by = 'STRAT', all.x = T)

  station.sums[, W.h := Area / sum(Area, na.rm = T), by = YEAR]

  x <- merge(x, station.sums, by = key(stations))

  #Sum biomass by species per area
  if(length(which(names(x) == 'CATCHSEX')) > 0){
    setkey(x,
          GROUP,
          CATCHSEX,
          STRAT,
          YEAR)
    }
  
  if(length(which(names(x) == 'CATCHSEX')) < 1){
    setkey(x,
          GROUP,
          STRAT,
          YEAR)
    }
  
    
  x[, biomass.tow := sum(BIOMASS)/ntows, by = key(x)]
  x[, abundance.tow := sum(ABUNDANCE)/ntows, by = key(x)]
  
  #Calculated stratified means
  x[, weighted.biomass := biomass.tow * W.h]
  x[, weighted.abundance := abundance.tow * W.h]

  #Variance
  x[, n.zero := ntows - count(BIOMASS), by = key(x)]
  x[, zero.var.b := n.zero * (0 - biomass.tow)^2]
  x[, vari.b := (BIOMASS - biomass.tow)^2]
  x[, Sh.2.b := (zero.var.b + sum(vari.b))/(ntows-1), by = key(x)]
  x[is.nan(Sh.2.b), Sh.2.b := 0]
  x[, zero.var.a := n.zero * (0 - abundance.tow)^2]
  x[, vari.a := (ABUNDANCE - abundance.tow)^2]
  x[, Sh.2.a := (zero.var.a + sum(vari.a))/(ntows-1), by = key(x)]
  x[is.nan(Sh.2.a), Sh.2.a := 0]
  
  stratified <- unique(x)
  
  if(length(which(names(x) == 'CATCHSEX')) > 0){
    setkey(stratified,
          GROUP,
          CATCHSEX,
          YEAR)
    }
  
  if(length(which(names(x) == 'CATCHSEX')) < 1){
    setkey(stratified,
          GROUP,
          YEAR)
    }
  
  #Stratified mean  
  stratified[, strat.biomass := sum(weighted.biomass, na.rm = T),   by = key(stratified)]
  stratified[, strat.abund   := sum(weighted.abundance, na.rm = T), by = key(stratified)]
  
  #Stratified variance
  stratified[, biomass.S2 := sum(Sh.2.b * W.h) / sum(ntows) + sum((1 - W.h) * Sh.2.b)/sum(ntows)^2, by = key(stratified)]
  stratified[, abund.S2   := sum(Sh.2.a * W.h) / sum(ntows) + sum((1 - W.h) * Sh.2.a)/sum(ntows)^2, by = key(stratified)]
  
  #standard error of the means
  stratified[, biomass.SE := sqrt(biomass.S2) / sqrt(sum(ntows)), by = key(stratified)]
  stratified[, abund.SE   := sqrt(abund.S2)   / sqrt(sum(ntows)), by = key(stratified)]
  
  #Delete extra rows/columns
  stratified.means <- unique(stratified)
  if(length(which(names(x) == 'CATCHSEX')) > 0){
    stratified.means <- stratified.means[, which(names(stratified.means) %in% c('YEAR', 'GROUP', 'CATCHSEX', 
                                                                                'strat.biomass', 'biomass.S2', 'biomass.SE',
                                                                                'strat.abund',   'abund.S2',   'abund.SE')), 
                                         with = F]
                                         
    setcolorder(stratified.means, c('YEAR', 'GROUP', 'CATCHSEX', 
                                    'strat.biomass', 'biomass.S2', 'biomass.SE',
                                    'strat.abund',   'abund.S2',   'abund.SE'))
    }
    
  if(length(which(names(x) == 'CATCHSEX')) < 1){
    stratified.means <- stratified.means[, which(names(stratified.means) %in% c('YEAR', 'GROUP', 
                                                                                'strat.biomass', 'biomass.S2', 'biomass.SE',
                                                                                'strat.abund',   'abund.S2',   'abund.SE')), 
                                         with = F]
    setcolorder(stratified.means, c('YEAR', 'GROUP', 
                                    'strat.biomass', 'biomass.S2', 'biomass.SE',
                                    'strat.abund',   'abund.S2',   'abund.SE'))
                                    
    }
    
  setnames(stratified.means, 'GROUP', groups)
  
  
  
  #return names of x back to originals
  setnames(x,
          c('STRAT', 'GROUP'),
          c(x.strata.col, groups))
  
  return(stratified.means)
  
  }