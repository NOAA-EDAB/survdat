#' Extracts Survey data  from Database
#'
#'Connects to svdbs and pulls data from MSTR_CRUISE, UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA, UNION_FSCS_SVBIO
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param filterByYear Numeric vector. Subset of years from which to pull data.
#'                     If not specified then all years are pulled. (Default = NA)
#' @param all.season Boolean. Spring and Fall only (F) otherwise T. (Default = F)
#' @param shg.check Boolean. use only SHG <=136 or TOGA <= 1324 (>2008). (Default = T)
#' @param conversion.factor Boolean. Whether to apply conversion factors to the data pull, (Default = T)
#' @param use.SAD Boolean. Use Survey Analysis Database (SAD) for assessed species. (Default = F)
#' @param getBio Boolean. Include biology data for each fish weight, sex,, stomach weight, stomach volume, age, maturity
#' @param getLengths Boolean. Include length data which includes the length in
#'                   cm and the number at length. (Default = T)
#'
#' @return A list containing a Data frame (data.table) (n x 21), a list of SQL queries used to pull the data,
#' the date of the pull, and the call expression
#' Each row of the data.table represents the number at length of a species on a specific tow along with physical attributes of the tow.
#'
#' The data frame (Descriptions taken from NEFSC Data dictionary)
#'
#' \item{CRUISE6}{Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year. The 5th byte signifies cruises other than groundfish: Shrimp survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e. 201491), Food habits = 5 (i.e.199554)}
#' \item{STATION}{Unique sequential order in which stations have been completed. Hangups and short tows each receive a non-repeated consecutive number.}
#' \item{STRATUM}{	A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes). Stratum group refers to if area fished is inshore or offshore North or South of Cape Hatteras or the type of cruise (shellfish, State of MA, offshore deepwater). The stratum number (third and fourth digits of code) refers to area defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the code increases the length of the stratum number for revised strata after the Hague Line was established. Stratum group code: 01 = Trawl, offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl, inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09 = MA DMF; 99 = Offshore deepwater (outside the stratified area). A change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has been in effect since Spring 1987, and may be summarized as follows: Previous strata: 01350; Present strata: 01351, 01352.}
#' \item{TOW}{Sequential number representing order in which station was selected within a stratum.}
#' \item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#' \item{CATCHSEX}{Code used to identify species that are sexed at the catch level. See SVDBS.SEX_CODES}
#' \item{SVVESSEL}{Standard two character code for a survey vessel. Refer to SVDBS.SV_VESSEL}
#' \item{YEAR}{	Year in which cruise was conducted.}
#' \item{SEASON}{	Season of the year in which cruise was conducted.}
#' \item{LAT}{Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)}
#' \item{LON}{Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)}
#' \item{EST_TOWDATE}{Date and time represented by Eastern Standard Time (EST) for the start of a tow or deployment.(BEGIN_EST_TOWDATE)}
#' \item{DEPTH}{	A four digit number recording the average depth, to the nearest meter, during a survey gear deployment.(AVGDEPTH)}
#' \item{SURFTEMP}{Surface temperature of water (degrees Celcius).}
#' \item{SURFSALIN}{Salinity at water surface in practical salinity units (PSU).}
#' \item{BOTTEMP}{Bottom temperature (degrees Celsius).}
#' \item{BOTSALIN}{Bottom salinity in Practical Salinity Units (PSU).}
#' \item{ABUNDANCE}{Expanded number of individuals of a species caught at a given station.(EXPCATCHNUM)}
#' \item{BIOMASS}{Expanded catch weight of a species caught at a given station. (EXPCATCHWT)}
#' \item{LENGTH}{Measured length of species in centimeters (cm). Measure method differs by species.}
#' \item{NUMLEN}{Expanded number of specimens at a given length.(EXPNUMLEN)}
#'
#' Additional columns if bio = T (UNION_FSCS_SVBIO)
#'
#' \item{INDID}{A unique identifier for each fish sampled.}
#' \item{INDWT}{Individual weight (KG) of species being sampled.}
#' \item{SEX}{Code indicating sex of fish or invertebrate species. See SVDBS.FSCS_SEX_CODES if using fscs data and SVDBS.SEX_CODES if using non FSCS data. Codes 0, 1, 2 and 4 are the only valid codes in fscs tables.}
#' \item{MATURITY}{Stage of maturation of the fish being sampled. See SVDBS.FSCS_MATURITY_CODES}
#' \item{AGE}{Age of specimen in years.}
#' \item{STOM_VOLUME}{Volume of the stomach contents of the fish sampled, measured to the nearest tenth of a cubic centimeter (cc).}
#' \item{STOM_WGT}{	Stomach weight of an individual fish in grams.}
#'
#' The list of sql statements:
#'
#' \item{cruise}{Select unique list of cruises. Table = MSTR_CRUISE}
#' \item{station}{Select unique set of stations from result of \code{cruise}. Table = UNION_FSCS_SVSTA}
#' \item{catch}{Select species abundance and biomass data from result of \code{station}. Table = UNION_FSCS_SVCAT}
#' \item{length}{Select Lengths of species found in \code{catch}. Table = UNION_FSCS_SVLEN}
#' \item{sad}{Select ????? from Survey Analysis Database. Table = STOCKEFF.I_SV_MERGED_CATCH_CALIB_O}
#' \item{conversions}{Select conversion factors. Table = SURVAN_CONVERSION_FACTORS}
#' \item{bio}{Select bio stats. Table = UNION_FSCS_SVBIO}
#'
#' The date:
#'
#'  \item{pullDate}{The date the data was pulled from the database}
#'
#' The expression:
#'
#' \item{functionCall}{The call used to create the data pul}
#'
#' @importFrom data.table "like"
#'
#'@family survdat
#'
#'@examples
#'\dontrun{
#'# First create a connection object to the database
#'channel <- dbutils::connect_to_database("serverName","userName")
#'# pull survey data, applies conversion factors (Door, net, vessel) and join with
#'# biological data to return individual sex, age, maturity, stomach data
#'get_survdat_data(channel,conversion.factor = T, bio=T)
#'
#' # Same data pull without individual biological data
#'get_survdat_data(channel,conversion.factor = T, bio=F)
#'}
#'
#'@export

get_survdat_data <- function(channel, filterByYear = NA, all.season = F,
                             shg.check = T, conversion.factor = T, use.SAD = F,
                             getBio = F, getLengths = T) {

  call <- capture_function_call()

  # Cruise List --------------------------------------------------------------
  #Generate cruise list
  message("Getting Cruise list  ...")

  #Create year vector
  if(is.na(filterByYear[1])){
    years <- ">= 1963"
  }else{
    years <- paste0("in (", survdat:::sqltext(filterByYear), ")")
  }


  if(all.season == F){ # Spring and Fall
    cruise.qry <- paste0("select unique year, cruise6, svvessel, season
      from mstr_cruise
      where purpose_code = 10
      and year ", years,
      "and (season = 'FALL'
        or season = 'SPRING')
      order by year, cruise6")
   } else if(all.season == T){ # Everything
    cruise.qry <- paste0("select unique year, cruise6, svvessel, season
      from mstr_cruise
      where purpose_code = 10
      and year ", years,
      "order by year, cruise6")
   }


  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  data.table::setkey(cruise, CRUISE6, SVVESSEL)

  #Use cruise codes to select other data
  cruise6 <- survdat:::sqltext(cruise$CRUISE6)



  # Station Data --------------------------------------------------------------
  message("Getting Station data ...")
  if(shg.check == T){
    station.qry <- paste0("select unique cruise6, svvessel, station, stratum,
                               tow, decdeg_beglat as lat, decdeg_beglon as lon,
                               begin_est_towdate as est_towdate, avgdepth as depth,
                               surftemp, surfsalin, bottemp, botsalin
                               from UNION_FSCS_SVSTA
                               where cruise6 in (", cruise6, ")
                               and (SHG <= 136 and cruise6 <= 200900)
                               or (TOGA <= 1324 and cruise6 > 200900)
                               order by cruise6, station")
  }
  if(shg.check == F){
    station.qry <- paste0("select unique cruise6, svvessel, station, stratum, tow,
                         decdeg_beglat as lat, decdeg_beglon as lon,
                         begin_est_towdate as est_towdate, avgdepth as depth,
                         surftemp, surfsalin, bottemp, botsalin
                         from UNION_FSCS_SVSTA
                         where cruise6 in (", cruise6, ")
                         order by cruise6, station")
  }
  # pull data
  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))

  data.table::setkey(station, CRUISE6, SVVESSEL)
  #merge cruise and station
  survdat <- merge(cruise, station)


  # Catch Data --------------------------------------------------------------
  message("Getting Species data ...")
  catch.qry <- paste0("select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from UNION_FSCS_SVCAT
                     where cruise6 in (", cruise6, ")
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp")
  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  data.table::setkey(catch, CRUISE6, STATION, STRATUM, TOW)

  #merge with survdat
  data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW)
  survdat <- merge(survdat, catch, by = data.table::key(survdat))

  # create list of all sql calls
  sql <- list(catch=catch.qry, cruise=cruise.qry, station=station.qry)



  # Length Data --------------------------------------------------------------
  if(getLengths){
    message("Getting Length Data ...")
    #Length data
    length.qry <- paste0("select cruise6, station, stratum, tow, svspp, catchsex,
                      length, expnumlen as numlen
                      from UNION_FSCS_SVLEN
                      where cruise6 in (", cruise6, ")
                      and stratum not like 'YT%'
                      order by cruise6, station, svspp, length")
    len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
    data.table::setkey(len, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)

    #merge with survdat
    data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)
    survdat <- merge(survdat, len, all.x = T)

    sql <- c(sql,length=length.qry)

  }


  # Biology Data --------------------------------------------------------------
  if (getBio) {
    message("Getting Individual Fish (Bio) Data ...")
    bio.qry <- paste0("select cruise6, station, stratum, svspp, catchsex, length, indid,
                  indwt, sex, maturity, age, stom_volume, stom_wgt
                  from UNION_FSCS_SVBIO
                  where cruise6 in (", cruise6, ")")
    bio <- data.table::as.data.table(DBI::dbGetQuery(channel, bio.qry))

    #Remove YT Stratum
    bio <- bio[!STRATUM %like% 'YT', ]

    # fix bugs in SVDBS for character sex values
    bio[SEX %in% c("M","m"), SEX := 1]
    bio[SEX %in% c("F","f"), SEX := 2]

    #Fix catch sex prior to 2001
    bio[is.na(CATCHSEX), CATCHSEX := 0]
    bio[SVSPP %in% c('015', '301') & SEX == 1 & CRUISE6 < 200100, CATCHSEX := 1]
    bio[SVSPP %in% c('015', '301') & SEX == 2 & CRUISE6 < 200100, CATCHSEX := 2]

    data.table::setkey(bio, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX, LENGTH)
    data.table::setkey(survdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX, LENGTH)

#    data.table::setkey(bio, CRUISE6, STATION, STRATUM, TOW, SVSPP)
#    data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW, SVSPP)
    survdat <- merge(survdat, bio, by = key(survdat))
    sql <- c(sql,bio=bio.qry)
  }

  # Apply conversion factors --------------------------------------------------
  if(conversion.factor){
    survdatConv <- apply_conversion_factors(channel,survdat,use.SAD = use.SAD)
    sql <- c(sql,survdatConv$sql)
    survdat <- survdatConv$survdat
  }

  #Convert number fields from chr to num
  numberCols <- c('CRUISE6', 'STATION', 'STRATUM', 'TOW', 'SVSPP', 'CATCHSEX', 'YEAR')
  survdat[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]



  return(list(survdat=survdat,sql=sql,pullDate=date(),functionCall = call))

}
