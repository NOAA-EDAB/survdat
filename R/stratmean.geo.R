stratmean.geo <- function (survdat, groups = 'all', group.col = 'SVSPP', 
                           merge.sex = T, sex.col = 'CATCHSEX', 
                           strat.col = 'STRATUM', poststrat = F, nsta.col = 'ntows', 
                           area.wgt = 'W.h', weight = 'BIOMASS', number = 'ABUNDANCE', 
                           constant = 0.1) {
  x <- copy(survdat)
  
  #Remove length data if present
  setkey(x, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX)
  x <- unique(x)
  x[, c('LENGTH', 'NUMLEN') := NULL]
  
  setnames(x, c(group.col, sex.col, strat.col, nsta.col, area.wgt, weight, number),
           c('group', 'sex', 'strat', 'ntows', 'W.h', 'BIO', 'NUM'))
  
  #Merge sex or keep seperate
  if(merge.sex == F) x[, group := paste(group, sex, sep = '')]
  
  setkey(x, CRUISE6, strat, STATION, group)
  x[, BIO := sum(BIO), by = key(x)]
  x[, NUM := sum(NUM), by = key(x)]
  x <- unique(x)
  
  #Fix Na's
  x[is.na(BIO), BIO := 0]
  x[is.na(NUM), NUM := 0]
  
  #Calculate total number of stations per year
  setkey(x, strat, YEAR)
  N <- unique(x)
  N <- N[, sum(ntows), by = 'YEAR']
  setnames(N, 'V1', 'N')
  
  #Subset data if necessary
  if(groups[1] != 'all'){
    if(merge.sex == F) groups <- c(paste(groups, 0, sep = ''), paste(groups, 1, sep = ''),
                                   paste(groups, 2, sep = ''), paste(groups, 3, sep = ''))
    x <- x[group %in% groups, ]
  }
  
  #Calculate weight per tow and number per tow
  setkey(x, group, strat, YEAR)
  
  x[, n.zero        := ntows - length(BIO), by = key(x)]
  x[, biomass.tow   := exp((sum(log(BIO + constant) + log(constant) * n.zero) / 
                              ntows)), by = key(x)]
  x[, abundance.tow := exp((sum(log(NUM + constant) + log(constant) * n.zero) / 
                              ntows)), by = key(x)]
  
  #Calculated stratified means
  x[, weighted.biomass   := biomass.tow   * W.h]
  x[, weighted.abundance := abundance.tow * W.h]
  
  #Variance - need to account for zero catch
  x[, zero.var.b := n.zero * (constant - biomass.tow)^2]
  x[, vari.b := (BIO - biomass.tow)^2]
  x[, Sh.2.b := (zero.var.b + sum(vari.b)) / (ntows - 1), by = key(x)]
  x[is.nan(Sh.2.b), Sh.2.b := 0]
  
  x[, zero.var.a := n.zero * (constant - abundance.tow)^2]
  x[, vari.a := (NUM - abundance.tow)^2]
  x[, Sh.2.a := (zero.var.a + sum(vari.a)) / (ntows - 1), by = key(x)]
  x[is.nan(Sh.2.a), Sh.2.a := 0]
  
  stratified <- unique(x)
  
  stratified <- merge(stratified, N, by = 'YEAR')
  
  #Stratified mean  
  setkey(stratified, group, YEAR)
  
  stratified[, strat.biomass := sum(weighted.biomass),   by = key(stratified)]
  stratified[, strat.abund   := sum(weighted.abundance), by = key(stratified)]
  
  #Stratified variance
  if(poststrat == F){
    stratified[, biomass.var := sum(((W.h^2) * Sh.2.b) / ntows), by = key(stratified)]
    stratified[, abund.var   := sum(((W.h^2) * Sh.2.a) / ntows), by = key(stratified)]
  }
  
  if(poststrat == T){
    stratified[, biomass.var := sum(Sh.2.b * W.h) / N + sum((1 - W.h) * Sh.2.b) / N^2, by = key(stratified)]
    stratified[, abund.var   := sum(Sh.2.a * W.h) / N + sum((1 - W.h) * Sh.2.a) / N^2, by = key(stratified)]
    
  }
  
  #standard error of the means
  stratified[, biomass.SE := sqrt(biomass.var), by = key(stratified)]
  stratified[, abund.SE   := sqrt(abund.var),   by = key(stratified)]
  
  #Delete extra rows/columns
  stratified.means <- unique(stratified)
  stratified.means <- stratified.means[, list(YEAR, group, sex, N, strat.biomass, biomass.var, biomass.SE, 
                                              strat.abund, abund.var, abund.SE)]
  if(merge.sex == T) stratified.means[, sex := NULL]
  
  if(merge.sex == F){
    stratified.means[, glen := nchar(group)]
    for(i in 2:4){
      stratified.means[glen == i, group := as.numeric(substr(group, 1, i - 1))]
    }
    stratified.means[, glen := NULL]
    setkey(stratified.means, YEAR, SVSPP, sex)
  }
  
  setnames(stratified.means, 'group', group.col)
  
  return(stratified.means)
}
