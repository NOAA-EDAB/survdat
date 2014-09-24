#Sweaptarea.r v.1.1
#Generate swept area biomass estimate from survey data
#3/14
#v 1.1 - forced renaming of both group and q columns in q variable
#SML

sweptarea <- function (prestrat.x, stratmean.x, q = NULL, a = 0.0384, strat.col, area.col, group.col = 'SVSPP') {
  #prestrat.x  <- data table output from prestrat
  #stratmean.x <- data table output from stratmean
  #q           <- data table of catchability coefficients (groups, q)
  #a           <- swept area (0.0384 km^2 is a standard Albatross IV tow)
  #strat.col   <- column of prestrat.x with the stratum names
  #area.col    <- column of prestrat.x with the stratum areas
  #group.col   <- column of stratmean.x which the mean is based on (i.e. svspp)

  #-------------------------------------------------------------------------------
  #Required packages
  library(data.table)
  
  #-------------------------------------------------------------------------------
  #User created functions
    
  #-------------------------------------------------------------------------------
  #This is necessary to break the link with the original data table
  prestrat.x  <- copy(prestrat.x)
  stratmean.x <- copy(stratmean.x)
  
  #Calculate A (Total area)
  setnames(prestrat.x, c(strat.col, area.col),
                       c('STRAT', 'S.AREA'))
  
  setkey(prestrat.x, YEAR, STRAT)
  stratum <- unique(prestrat.x)
  stratum <- stratum[, sum(S.AREA), by = 'YEAR']
  setnames(stratum, "V1", "A")
  
  #Merge A
  swept.area <- merge(stratmean.x, stratum, by = 'YEAR')
  
  #Merge q
  if(is.null(q)) q <- data.table(groups = unique(swept.area[, get(group.col)]), q = 1)
  setnames(q, names(q), c(group.col, 'q'))
  swept.area <- merge(swept.area, q, by = group.col)
  
  #Calculate swept area biomass
  swept.area[, Tot.biomass   :=       (strat.biomass * A/a)/q]
  swept.area[, Tot.abundance := round((strat.abund   * A/a)/q)]
  
  #remove extra columns
  swept.area[, c('A', 'q') := NULL]

  return(swept.area)
  }