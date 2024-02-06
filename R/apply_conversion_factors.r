#' Applies conversion factors to Raw Survey data pull
#'
#' Connects to svdbs and pulls conversion factors from SURVAN_CONVERSION_FACTORS
#' Explain what conversions do ...
#'
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param survdat.raw Data.table. Result of \code{\link{get_survdat_data}}
#' @param use.SAD Boolean. Use Survey Analysis Database (SAD) for assessed species. Default = F
#'
#' @return A list containing a Data frame (data.table) (n x 21) and a list of SQL queries used to pull the data
#' Each row of the data.table represents the number at length of a species on a specific tow along with physical attributes of the tow.
#' See \code{\link{get_survdat_data}} for more details
#'
#' @section Internal:
#' This is an internal function.
#' To pull the values of the conversion factors use \code{\link{get_conversion_factors}}
#'




apply_conversion_factors <- function(channel,survdat.raw,use.SAD = F) {

  survdat <- survdat.raw

  #Conversion Factors
  #need to make abundance column a double instead of an integer
  survdat[, ABUNDANCE := as.double(ABUNDANCE)]

  #Grab all conversion factors off the network
  convert.qry <- "select *
    from svdbs.survan_conversion_factors"

  convert <- data.table::as.data.table(DBI::dbGetQuery(channel, convert.qry))

  #DCF < 1985 Door Conversion
  dcf.spp <- convert[DCF_WT > 0, SVSPP]
  for(i in 1:length(dcf.spp)){
    survdat[YEAR < 1985 & SVSPP == dcf.spp[i],
        BIOMASS := BIOMASS * convert[SVSPP == dcf.spp[i], DCF_WT]]
    }
  dcf.spp <- convert[DCF_NUM > 0, SVSPP]
  for(i in 1:length(dcf.spp)){
    survdat[YEAR < 1985 & SVSPP == dcf.spp[i],
        ABUNDANCE := round(ABUNDANCE * convert[SVSPP == dcf.spp[i], DCF_NUM])]
    }

  #GCF Spring 1973-1981  Net Conversion
  gcf.spp <- convert[GCF_WT > 0, SVSPP]
  for(i in 1:length(gcf.spp)){
    survdat[SEASON == 'SPRING' & YEAR > 1972 & YEAR < 1982 & SVSPP == gcf.spp[i],
        BIOMASS := BIOMASS / convert[SVSPP == gcf.spp[i], GCF_WT]]
    }
  gcf.spp <- convert[GCF_NUM > 0, SVSPP]
  for(i in 1:length(gcf.spp)){
    survdat[SEASON == 'SPRING' & YEAR > 1972 & YEAR < 1982 & SVSPP == gcf.spp[i],
        ABUNDANCE := round(ABUNDANCE / convert[SVSPP == gcf.spp[i], GCF_NUM])]
    }

  #VCF SVVESSEL = DE  Vessel Conversion
  vcf.spp <- convert[VCF_WT > 0, SVSPP]
  for(i in 1:length(vcf.spp)){
    survdat[SVVESSEL == 'DE' & SVSPP == vcf.spp[i],
        BIOMASS := BIOMASS * convert[SVSPP == vcf.spp[i], VCF_WT]]
    }
  vcf.spp <- convert[VCF_NUM > 0, SVSPP]
  for(i in 1:length(vcf.spp)){
    survdat[SVVESSEL == 'DE' & SVSPP == vcf.spp[i],
        ABUNDANCE := round(ABUNDANCE * convert[SVSPP == vcf.spp[i], VCF_NUM])]
    }

  #Bigelow >2008 Vessel Conversion
  #Use Bigelow conversions for Pisces as well (PC)
  #Tables 53-55 from Miller et al. 2010 - number estimators
  big.abund <- data.table::data.table(svspp = c('012', '022', '024', '027', '028',
                                                '031', '033', '034', '073', '076',
                                                '106', '107', '109', '121', '135',
                                                '136', '141', '143', '145', '149',
                                                '155', '164', '171', '181', '193',
                                                '197', '502', '512', '015', '023',
                                                '026', '032', '072', '074', '077',
                                                '078', '102', '103', '104', '105',
                                                '108', '131', '163', '301', '313',
                                                '401', '503', '015', '023', '026',
                                                '032', '072', '074', '077', '078',
                                                '102', '103', '104', '105', '108',
                                                '131', '163', '301', '313', '401',
                                                '503'),
                          season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                          rho = c(1.161, 4.44, 6.689, 4.384, 3.792, 1.227, 1.323, 1.29,
                                  1.987, 2.235, 2.49, 3.257, 8.249, 1.188, 1.16, 1.134,
                                  3.416, 1.705, 1.577, 1.541, 1.456, 1.802, 4.494, 0.57,
                                  4.575, 7.129,	1.38, 2.309, 1.202, 3.822, 3.08, 2.287,
                                  6.283, 0.972, 3.959, 3.839, 2.074, 3.226, 3.099, 2.347,
                                  3.311, 1.487, 3.56, 1.571, 3.343, 3.965, 2.034, 1.204,
                                  2.609, 9.846, 2, 4.354, 1.816, 2.662, 2.319, 2.16,
                                  2.405, 2.356, 2.366, 2.044, 1.935, 3.114, 1.586, 2.511,
                                  3.166, 0.84))

  #Tables 56-58 from Miller et al. 2010 Biomass estimators
  big.bio <- data.table::data.table(svspp = c('012', '022', '024', '027', '028',
                                              '031', '033', '034', '073', '076',
                                              '106', '107', '109', '121', '135',
                                              '136', '141', '143', '145', '149',
                                              '155', '164', '171', '181', '193',
                                              '197', '502', '512', '015', '023',
                                              '026', '032', '072', '074', '077',
                                              '078', '102', '103', '104', '105',
                                              '108', '131', '163', '301', '313',
                                              '401', '503', '015', '023', '026',
                                              '032', '072', '074', '077', '078',
                                              '102', '103', '104', '105', '108',
                                              '131', '163', '301', '313', '401',
                                              '503'),
                        season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                        rhoW = c(1.082, 3.661, 6.189, 4.45, 3.626, 1.403, 1.1, 2.12,
                                 1.58, 2.088, 2.086, 3.257, 12.199, 0.868, 0.665, 1.125,
                                 2.827, 1.347, 1.994, 1.535, 1.191, 1.354, 3.259, 0.22,
                                 3.912, 8.062, 1.409, 2.075, 1.166, 3.718, 2.786, 5.394,
                                 4.591, 0.878, 3.712, 3.483, 2.092, 3.066, 3.05, 2.244,
                                 3.069, 2.356, 2.986, 1.272, 3.864, 1.85, 2.861, 1.21,
                                 2.174, 8.814, 1.95, 4.349, 1.489, 3, 2.405, 1.692,
                                 2.141, 2.151, 2.402, 1.901, 1.808, 2.771, 1.375, 2.479,
                                 3.151, 1.186))

  #ID species to convert
  spp.both <- big.abund[season == 'both', svspp]
  spp.seasonal <- big.abund[season == 'fall', svspp]

  #Both seasons
  for(ispp in 1:length(spp.both)){
    survdat[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp.both[ispp],
            BIOMASS := BIOMASS / big.bio[svspp == spp.both[ispp], rhoW]]
    survdat[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp.both[ispp],
            ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.both[ispp], rho])]
  }
  #Seperate calibration factors by season
  for(ispp in 1:length(spp.seasonal)){
    survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'SPRING' & SVSPP == spp.seasonal[ispp],
            BIOMASS := BIOMASS / big.bio[svspp == spp.seasonal[ispp] & season == 'spring' ,
                                         rhoW]]

    survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'FALL' & SVSPP == spp.seasonal[ispp],
            BIOMASS := BIOMASS / big.bio[svspp == spp.seasonal[ispp] & season == 'fall' ,
                                         rhoW]]

    survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'SPRING' & SVSPP == spp.seasonal[ispp],
            ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.seasonal[ispp] &
                                                       season == 'spring' , rho])]

    survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'FALL' & SVSPP == spp.seasonal[ispp],
            ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.seasonal[ispp] &
                                                       season == 'fall' , rho])]
    }



  #Need to update...no longer SAD now ADIOS.  Table structure has changed
  if(use.SAD == T){
    sad.qry <- "select svspp, cruise6, stratum, tow, station, sex as catchsex,
               catch_wt_B_cal, catch_no_B_cal, length, length_no_B_cal
               from STOCKEFF.I_SV_MERGED_CATCH_CALIB_O"

    sad <- data.table::as.data.table(DBI::dbGetQuery(channel, sad.qry))

    data.table::setkey(sad, CRUISE6, STRATUM, TOW, STATION, SVSPP, CATCHSEX, LENGTH)
    sad <- unique(sad)
    survdat <- merge(survdat, sad, by = key(sad), all.x = T)

    #Carry over SAD values to survdat columns and delete SAD columns
    survdat[!is.na(CATCH_WT_B_CAL),  BIOMASS   := CATCH_WT_B_CAL]
    survdat[!is.na(CATCH_NO_B_CAL),  ABUNDANCE := CATCH_NO_B_CAL]
    survdat[, NUMLEN := as.double(NUMLEN)]
    survdat[!is.na(LENGTH_NO_B_CAL), NUMLEN    := LENGTH_NO_B_CAL]
    survdat[, c('CATCH_WT_B_CAL', 'CATCH_NO_B_CAL', 'LENGTH_NO_B_CAL') := NULL]
  } else {
    sad.qry <- ""
  }

  sql <- list(sad=sad.qry,conversions=convert.qry)

  return(list(survdat=survdat,sql=sql))

  #if(all.season == 'n') save(survdat, file = here('data/Survdat.RData'))
  #if(all.season == 'y') save(survdat, file = here('data/Survdat_allseason.RData'))
}



