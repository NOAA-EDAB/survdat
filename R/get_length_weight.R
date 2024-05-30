#' Extract species specific LENGTH-WEIGHT COEFFICIENTS from SVDBS
#'
#'Pulls the length-weight coefficients from SVDBS LENGTH_WEIGHT_COEFFICIENTS table
#' These coefficients are described in NOAA Tech Memo NMFS-NE-171.
#'
#'
#' @inheritParams get_survdat_data
#'
#'@return Returns a data.table (nx12).
#'
#'\item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#'\item{SVLWEXP}{The exponent of the length-weight equation, b.}
#'\item{SVLWCOEFF}{The natural log of the coefficient of the length-weight equation, ln a.}
#'
#'@family survdat
#'
#'@export


get_length_weight <- function(channel){

  #Grab survey length/weight coefficients
  lw.qry <- "select svspp, sex, svlwexp, svlwcoeff, svlwexp_spring, svlwcoeff_spring,
               svlwexp_fall, svlwcoeff_fall, svlwexp_winter, svlwcoeff_winter,
               svlwexp_summer, svlwcoeff_summer
               from svdbs.length_weight_coefficients"

  lw <- data.table::as.data.table(DBI::dbGetQuery(channel, lw.qry))

  #Clean up NAs
  lw <- lw[!is.na(SVLWEXP), ]
  lw[is.na(SVLWEXP_SPRING), SVLWEXP_SPRING := SVLWEXP]
  lw[is.na(SVLWEXP_FALL),   SVLWEXP_FALL   := SVLWEXP]
  lw[is.na(SVLWEXP_WINTER), SVLWEXP_WINTER := SVLWEXP]
  lw[is.na(SVLWEXP_SUMMER), SVLWEXP_SUMMER := SVLWEXP]
  lw[is.na(SVLWCOEFF_SPRING), SVLWCOEFF_SPRING := SVLWCOEFF]
  lw[is.na(SVLWCOEFF_FALL),   SVLWCOEFF_FALL   := SVLWCOEFF]
  lw[is.na(SVLWCOEFF_WINTER), SVLWCOEFF_WINTER := SVLWCOEFF]
  lw[is.na(SVLWCOEFF_SUMMER), SVLWCOEFF_SUMMER := SVLWCOEFF]

  #Make output amendable to survdat
  data.table::setnames(lw, "SEX", "CATCHSEX")

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'LENGTH_WEIGHT_COEFFICIENTS' and owner='SVDBS'"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=lw[],sql=lw.qry, colNames=colNames))

}
