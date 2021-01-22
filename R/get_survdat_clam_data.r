#' Extracts Clam data from Survey Database
#'
#'Connects to svdbs and pulls Clam & Quahog data from MSTR_CRUISE, UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA.
#'Pulls from Cruises with purpose code = 50. (See \code{\url{get_cruise_purpose}}). Data are assigned to one of 7 regions
#'('SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB') and length-to-meat weight conversions applied
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param shg.check Boolean. use only SHG <=136 or TOGA <= 1324 (>2008). (Default = T)
#' @param clam.only Boolean. T = grab only Atl. surfclam (403) and ocean quahog (409)
#' @param tidy Boolean. Return output in long format (Default = F)
#'
#' @return A list containing a Data frame (data.table) (n x 21) and a list of SQL queries used to pull the data, the date of the pull, and the call expression
#' Each row of the data.table represents the number at length of a species on a specific tow along with physical attributes of the tow.
#'
#' The data frame (Descriptions taken from NEFSC Data dictionary)
#'
#' \item{clam.region}{One of 7 identified Regions, 'SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB'}
#' \item{CRUISE6}{Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year. The 5th byte signifies cruises other than groundfish: Shrimp survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e. 201491), Food habits = 5 (i.e.199554)}
#' \item{STATION}{Unique sequential order in which stations have been completed. Hangups and short tows each receive a non-repeated consecutive number.}
#' \item{STRATUM}{	A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes). Stratum group refers to if area fished is inshore or offshore North or South of Cape Hatteras or the type of cruise (shellfish, State of MA, offshore deepwater). The stratum number (third and fourth digits of code) refers to area defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the code increases the length of the stratum number for revised strata after the Hague Line was established. Stratum group code: 01 = Trawl, offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl, inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09 = MA DMF; 99 = Offshore deepwater (outside the stratified area). A change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has been in effect since Spring 1987, and may be summarized as follows: Previous strata: 01350; Present strata: 01351, 01352.}
#' \item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#' \item{CATCHSEX}{Code used to identify species that are sexed at the catch level. See SVDBS.SEX_CODES}
#' \item{SVVESSEL}{Standard two character code for a survey vessel. Refer to SVDBS.SV_VESSEL}
#' \item{YEAR}{	Year in which cruise was conducted.}
#' \item{LAT}{Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)}
#' \item{LON}{Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)}
#' \item{DEPTH}{	A four digit number recording the average depth, to the nearest meter, during a survey gear deployment.(AVGDEPTH)}
#' \item{SURFTEMP}{Surface temperature of water (degrees Celcius).}
#' \item{SURFSALIN}{Salinity at water surface in practical salinity units (PSU).}
#' \item{BOTTEMP}{Bottom temperature (degrees Celsius).}
#' \item{BOTSALIN}{Bottom salinity in Practical Salinity Units (PSU).}
#' \item{ABUNDANCE}{Expanded number of individuals of a species caught at a given station.(EXPCATCHNUM)}
#' \item{BIOMASS}{Expanded catch weight of a species caught at a given station. (EXPCATCHWT)}
#' \item{LENGTH}{Measured length of species in centimeters (cm). Measure method differs by species.}
#' \item{NUMLEN}{Expanded number of specimens at a given length.(EXPNUMLEN)}
#' \item{BIOMASS.MW}{Meat weight of catch based on LENGTH and NUMLEN of clams, conversion factors hard coded}
#'
#'
#' The list of sql statements:
#'
#' \item{cruise}{Select unique list of cruises. Table = MSTR_CRUISE}
#' \item{station}{Select unique set of stations from result of \code{cruise}. Table = UNION_FSCS_SVSTA}
#' \item{catch}{Select species abundance and biomass data from result of \code{station}. Table = UNION_FSCS_SVCAT}
#' \item{length}{Select Lengths of species found in \code{catch}. Table = UNION_FSCS_SVLEN}
#'
#' The date:
#'
#'  \item{pullDate}{The date the data was pulled from the database}
#'
#' The expression:
#'
#' \item{functionCall}{The call used to create the data pul}
#'
#'
#'@family survdat
#'
#'@examples
#'\dontrun{
#'# Recommended use:
#'channel <- dbutils::connect_to_database("serverName","userName")
#'get_survdat_clam_data(channel)
#'
#'}
#'
#'@export


#-------------------------------------------------------------------------------
#User parameters

get_survdat_clam_data <- function(channel,
                                  shg.check=T,
                                  clam.only=T,
                                  tidy = F) {

  call <- capture_function_call()

  #Generate cruise list
  #V1.2 - remove surveys prior to 1982 due to difference in seasons/gear
  cruise.qry <- "select unique year, cruise6, svvessel
                 from mstr_cruise
                 where purpose_code = 50
                 and year >= 1982
                 order by year, cruise6"

  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  data.table::setkey(cruise, CRUISE6, SVVESSEL)

  #Use cruise codes to select other data
  cruise6 <- survdat:::sqltext(cruise$CRUISE6)


  #Station data
  if(shg.check == T){
    station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from Union_fscs_svsta
                   where cruise6 in (", cruise6, ")
                   and SHG <= 136
                   order by cruise6, station", sep='')
    } else {
    station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from UNION_FSCS_SVSTA
                   where cruise6 in (", cruise6, ")
                   order by cruise6, station", sep='')
    }

  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  data.table::setkey(station, CRUISE6, SVVESSEL)

  #merge cruise and station
  clamdat <- base::merge(cruise, station)

  #Catch data
  if(clam.only == T){
    catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 and svspp in ('403', '409')
                 order by cruise6, station, svspp", sep='')
    } else {
    catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 order by cruise6, station, svspp", sep='')
    }

  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  data.table::setkey(catch, CRUISE6, STATION, STRATUM)

  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM)
  clamdat <- base::merge(clamdat, catch, all.x = T)

  #Length data
  if(clam.only == T){
    length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  and svspp in ('403', '409')
                  order by cruise6, station, svspp, length", sep='')
  } else {
    length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  order by cruise6, station, svspp, length", sep='')
  }

  len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
  data.table::setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
  clamdat <- base::merge(clamdat, len, all.x = T)

  clamdat[,STRATUM := as.numeric(STRATUM)]

  #Assign clam regions
  regions <- c('SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB')
  SVA <- c(6010:6080, 6800, 6810)
  DMV <- c(6090:6160, 6820:6860)
  SNJ <- c(6170:6200, 6870)
  NNJ <- c(6210:6280, 6880:6900)
  LI  <- c(6290:6360, 6910:6930)
  SNE <- c(6370:6520, 6940:6960)
  GB  <- c(6530:6740)

  clamdat[, clam.region := factor(NA, levels = regions)]
  for(i in 1:length(regions)) clamdat[STRATUM %in% get(regions[i]), clam.region := regions[i]]

  #shell length-to-meat weight conversion coefficients (OQ NEFSC 2004, SC NEFSC 2003)
  coeff <- data.table::data.table(clam.region = c('SVA',     'DMV',     'SNJ',     'NNJ',     'LI',      'SNE',     'GB'),
                      oq.a        = c(-9.04231,  -9.04231,  -9.84718,  -9.84718,  -9.23365,  -9.12428,  -8.96907),
                      oq.b        = c( 2.787987,  2.787987,  2.94954,   2.94954,   2.822474,  2.774989,  2.767282),
                      sc.a        = c(-7.0583,   -9.48913,  -9.3121,   -9.3121,   -7.9837,   -7.9837,   -8.27443),
                      sc.b        = c( 2.3033,    2.860176,  2.863716,  2.863716,  2.5802,    2.5802,    2.654215))
  coeff[, clam.region := as.factor(clam.region)]
  clamdat <- base::merge(clamdat, coeff, by = 'clam.region')

  #Lengths need to be in mm for formula to give g.  Divide by 1000 to get results in kg
  clamdat[SVSPP == 403, meatwt := (exp(sc.a) * (LENGTH * 10) ^ sc.b) / 1000]
  clamdat[SVSPP == 409, meatwt := (exp(oq.a) * (LENGTH * 10) ^ oq.b) / 1000]
  clamdat[, expmw := meatwt * NUMLEN]
  clamdat[, stamw := sum(expmw), by = c('CRUISE6', 'STRATUM', 'STATION', 'SVSPP')]
  clamdat[, c('oq.a', 'oq.b', 'sc.a', 'sc.b', 'meatwt', 'expmw') := NULL]
  data.table::setnames(clamdat, "stamw", "BIOMASS.MW")

  #saveRDS(clamdat, file = here::here('data","Clamdat.RDS'))
  if (tidy) {
    clamdat <- tibble::as_tibble(clamdat)
  }

  sql <- list(cruise=cruise.qry,station=station.qry,catch=catch.qry,length=length.qry)

  return(list(data=clamdat,sql=sql,pullDate=date(),functionCall = call))
}
