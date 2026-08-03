#' Extracts Clam data from Survey Database
#'
#'Connects to svdbs and pulls Clam & Quahog data from MSTR_CRUISE, UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA.
#'Pulls from Cruises with purpose code = 50. (See \code{\url{get_cruise_purpose}}). Data are assigned to one of 2 new regions
#'('South', 'GBK') and length-to-meat weight conversions applied.
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param shg.check Boolean. use only SHG <=136 or TOGA <= 1324 (>2008). (Default = T)
#' @param clam.only Boolean. T = grab only Atl. surfclam (403) and ocean quahog (409)
#' @param tidy Boolean. Return output in long format (Default = F)
#' @param assignRegionWeights Boolean. Assign Strata to Regions and then apply length weight coefficients. (Default = T).
#'
#' @return A list containing a Data frame (data.table) (n x 21) and a list of SQL queries used to pull the data, the date of the pull, and the call expression
#'
#'@export

#-------------------------------------------------------------------------------
#User parameters

get_survdat_clam_data <- function(
    channel,
    shg.check = T,
    clam.only = T,
    tidy = F,
    assignRegionWeights = T
) {
  call <- capture_function_call()
  
  #Generate cruise list
  cruise.qry <- "select unique year, cruise6, svvessel
                 from svdbs.mstr_cruise
                 where purpose_code = 50
                 and year >= 1982
                 order by year, cruise6"
  
  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  data.table::setkey(cruise, CRUISE6, SVVESSEL)
  
  #Use cruise codes to select other data
  cruise6 <- sqltext(cruise$CRUISE6)
  
  #Station data
  if (shg.check == T) {
    station.qry <- paste(
      "select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from svdbs.Union_fscs_svsta
                   where cruise6 in (", cruise6, ")
                   and SHG <= 136
                   order by cruise6, station", sep = ''
    )
  } else {
    station.qry <- paste(
      "select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from svdbs.UNION_FSCS_SVSTA
                   where cruise6 in (", cruise6, ")
                   order by cruise6, station", sep = ''
    )
  }
  
  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  data.table::setkey(station, CRUISE6, SVVESSEL)
  
  #merge cruise and station
  clamdat <- base::merge(cruise, station)
  
  #Catch data
  if (clam.only == T) {
    catch.qry <- paste(
      "select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from svdbs.UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 and svspp in ('403', '409')
                 order by cruise6, station, svspp", sep = ''
    )
  } else {
    catch.qry <- paste(
      "select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from svdbs.UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 order by cruise6, station, svspp", sep = ''
    )
  }
  
  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  data.table::setkey(catch, CRUISE6, STATION, STRATUM)
  
  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM)
  clamdat <- base::merge(clamdat, catch, all.x = T)
  
  #Length data
  if (clam.only == T) {
    length.qry <- paste(
      "select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from svdbs.UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  and svspp in ('403', '409')
                  order by cruise6, station, svspp, length", sep = ''
    )
  } else {
    length.qry <- paste(
      "select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from svdbs.UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  order by cruise6, station, svspp, length", sep = ''
    )
  }
  
  len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
  data.table::setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
  
  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
  clamdat <- base::merge(clamdat, len, all.x = T)
  
  
  if (assignRegionWeights) {
    
    # 1. Clean the base stratum safely
    clamdat[, calc_strat := as.character(STRATUM)]
    # ONLY strip leading 6 and trailing 0 if it is an old 4+ digit shellfish stratum (e.g. 6170, 6010)
    # This protects the modern 2018+ strata (which already look like "1S", "2Q") from being corrupted
    clamdat[nchar(calc_strat) >= 4 & grepl("^0?6", calc_strat), 
            calc_strat := gsub("0$", "", gsub("^0?6", "", calc_strat))]
    
    clamdat[, sv_year := floor(as.numeric(CRUISE6) / 100)]
    
    # 2. Geometric Stratum Splits (Pre-2018)
    clamdat[calc_strat == '47', 
            calc_strat := data.table::fifelse(((LON - 69.23) * (41 - 40) - (LAT - 40) * (69.03 - 69.23)) > 0, '471', '472')]
    
    clamdat[calc_strat == '73', 
            calc_strat := data.table::fifelse(((LON - 66.8) * (41.9 - 41.35) - (LAT - 41.35) * (67.5 - 66.8)) > 0, '73', '74')]
    
    clamdat[SVSPP == 409 & calc_strat %in% c('25', '26') & LAT >= 39.3 & LAT <= 40.2 & 
              (((LON - 72) * (40.2 - 39.3) - (LAT - 39.3) * (73.75 - 72)) < 0), 
            calc_strat := data.table::fifelse(calc_strat == '26', '30', '29')]
    
    clamdat[SVSPP == 409 & calc_strat %in% c('31', '32') & 
              (((LON - 72) * (40.2 - 39.3) - (LAT - 39.3) * (73.75 - 72)) < 0), 
            calc_strat := data.table::fifelse(calc_strat == '31', '27', '28')]
    
    clamdat[SVSPP == 409 & calc_strat %in% c('25', '26') & LAT >= 40.2 & LAT <= 40.25 & 
              (((LON - 73.75) * (40.25 - 40.2) - (LAT - 40.25) * (73.775 - 73.75)) < 0), 
            calc_strat := data.table::fifelse(calc_strat == '25', '29', '30')]
    
    clamdat[SVSPP == 409 & calc_strat %in% c('25', '26') & LAT >= 40.25 & LAT <= 40.5 & 
              (((LON - 73.775) * (40.5 - 40.25) - (LAT - 40.25) * (73.825 - 73.775)) < 0), 
            calc_strat := data.table::fifelse(calc_strat == '25', '29', '30')]
    
    clamdat[SVSPP == 409 & calc_strat == '17' & 
              (((LON - 74.29) * (38.6 - 38.94) - (LAT - 38.94) * (74.57 - 74.29)) < 0), 
            calc_strat := '0']
    
    clamdat[SVSPP == 409 & calc_strat == '13' & LAT >= 38.41 & 
              (((LON - 74.57) * (38.41 - 38.6) - (LAT - 38.6) * (74.64 - 74.57)) < 0), 
            calc_strat := '0']
    
    clamdat[SVSPP == 409 & calc_strat == '13' & LAT >= 38.15 & LAT <= 38.41 & 
              (((LON - 74.64) * (38.15 - 38.41) - (LAT - 38.41) * (74.67 - 74.64)) < 0), 
            calc_strat := '0']
    
    clamdat[SVSPP == 409 & calc_strat == '13' & LAT <= 38.15 & 
              (((LON - 74.67) * (37.83 - 38.15) - (LAT - 38.15) * (74.87 - 74.67)) < 0), 
            calc_strat := '0']
    
    # 3. Assign New Strata for Pre-2018
    clamdat[, new_stratum := calc_strat] 
    
    # Surfclams Pre-2018
    clamdat[SVSPP == 403 & sv_year < 2018, new_stratum := data.table::fcase(
      calc_strat %in% c('05','09','81'), "1S",
      calc_strat %in% c('84', '85', '86', '87'), "2S",
      calc_strat %in% c('13', '17', '21', '25', '29'), "3S",
      calc_strat %in% c('10', '14', '18', '22'), "4S",
      calc_strat %in% c('88', '89', '90', '91', '92', '93'), "5S",
      calc_strat %in% c('45', '46', '95', '96'), "6S",
      calc_strat %in% c('53', '54'), "7S",
      calc_strat %in% c('67', '69', '70'), "8S",
      calc_strat %in% c('57', '58', '59', '60'), "9S",
      calc_strat %in% c('65', '66'), "10S",
      calc_strat %in% c('68', '72', '73'), "11S",
      calc_strat %in% c('71', '74'), "12S",
      default = "0"
    )]
    
    # Quahogs Pre-2018
    clamdat[SVSPP == 409 & sv_year < 2018, new_stratum := data.table::fcase(
      calc_strat %in% c('10', '11', '12', '14', '15', '16', '18', '19', '20'), "1Q",
      calc_strat %in% c('13', '17', '21', '22', '25', '26'), "2Q",
      calc_strat %in% c('23', '24', '27', '28', '31', '32', '35', '36'), "3Q",
      calc_strat %in% c('29', '30', '33', '34'), "4Q",
      calc_strat %in% c('92', '93', '94', '95', '37', '41'), "5Q",
      calc_strat %in% c('38', '39', '40', '46', '471', '48'), "6Q",
      calc_strat %in% c('53','54','55','56','472','56'), "7Q",
      calc_strat %in% c('70'), "8Q",
      calc_strat %in% c('57','58','59','60'), "9Q",
      calc_strat %in% c('65','66'), "10Q",
      calc_strat %in% c('74'), "11Q",
      calc_strat %in% c('61','62'), "12Q",
      default = "0"
    )]
    
    clamdat[!is.na(DEPTH) & DEPTH > 80, new_stratum := '0']
    
    # 4. Map to Assessment Regions (South vs GBK)
    clamdat[, clam.region := data.table::fcase(
      new_stratum %in% c("1S","2S","3S","4S","5S","6S","1Q","2Q","3Q","4Q","5Q","6Q"), "South",
      new_stratum %in% c("7S","8S","9S","10S","11S","12S","7Q","8Q","9Q","10Q","11Q","12Q"), "GBK",
      default = NA_character_
    )]
    
    # Clean up intermediate geometric columns
    clamdat[, c('calc_strat', 'sv_year', 'new_stratum') := NULL]
    
    # 5. Apply Meat Weight Coefficients
    coeff <- data.table::data.table(
      clam.region = c('South', 'GBK'),
      oq.a = c(-9.35615, -8.96907),
      oq.b = c(2.84542, 2.767282),
      sc.a = c(-8.52317, -8.27443),
      sc.b = c(2.675218, 2.654215)
    )
    
    coeff[, clam.region := as.factor(clam.region)]
    clamdat <- base::merge(clamdat, coeff, by = 'clam.region', all.x = TRUE)
    
    #Lengths need to be in mm for formula to give g.  Divide by 1000 to get results in kg
    clamdat[SVSPP == 403, meatwt := (exp(sc.a) * (LENGTH * 10)^sc.b) / 1000]
    clamdat[SVSPP == 409, meatwt := (exp(oq.a) * (LENGTH * 10)^oq.b) / 1000]
    clamdat[, expmw := meatwt * NUMLEN]
    clamdat[, stamw := sum(expmw, na.rm = TRUE), by = c('CRUISE6', 'STRATUM', 'STATION', 'SVSPP')]
    
    clamdat[, c('oq.a', 'oq.b', 'sc.a', 'sc.b', 'meatwt', 'expmw') := NULL]
    data.table::setnames(clamdat, "stamw", "BIOMASS.MW")
  }
  
  if (tidy) {
    clamdat <- tibble::as_tibble(clamdat)
  }
  
  sql <- list(
    cruise = cruise.qry,
    station = station.qry,
    catch = catch.qry,
    length = length.qry
  )
  
  return(list(
    data = clamdat,
    sql = sql,
    pullDate = date(),
    functionCall = call
  ))
}