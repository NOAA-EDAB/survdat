#' Extracts Massachusetts inshore bottom trawl survey data
#'
#' @description
#' Pulls survey data
#'
#' @inheritParams get_survdat_data
#'
#'@return A list containing a Data frame (data.table) (n x 19),
#' a list of SQL queries used to pull the data, the date of the pull,
#' and the call expression. Each row of the data.table represents the catch
#' of a species on a specific tow along with physical attributes of the tow.
#'
#' The data frame (Descriptions taken from NEFSC Data dictionary)
#'
#' \item{CRUISE6}{Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year. The 5th byte signifies cruises other than groundfish: Shrimp survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e. 201491), Food habits = 5 (i.e.199554)}
#' \item{STATION}{Unique sequential order in which stations have been completed. Hangups and short tows each receive a non-repeated consecutive number.}
#' \item{STRATUM}{	A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes). Stratum group refers to if area fished is inshore or offshore North or South of Cape Hatteras or the type of cruise (shellfish, State of MA, offshore deepwater). The stratum number (third and fourth digits of code) refers to area defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the code increases the length of the stratum number for revised strata after the Hague Line was established. Stratum group code: 01 = Trawl, offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl, inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09 = MA DMF; 99 = Offshore deepwater (outside the stratified area). A change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has been in effect since Spring 1987, and may be summarized as follows: Previous strata: 01350; Present strata: 01351, 01352.}
#' \item{TOW}{Sequential number representing order in which station was selected within a stratum.}
#' \item{SVVESSEL}{Standard two character code for a survey vessel. Refer to SVDBS.SV_VESSEL}
#' \item{YEAR}{	Year in which cruise was conducted.}
#' \item{SEASON}{	Season of the year in which cruise was conducted.}
#' \item{LAT}{Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)}
#' \item{LON}{Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)}
#' \item{EST_TOWDATE}{Date and time represented by Eastern Standard Time (EST) for the start of a tow or deployment.(BEGIN_EST_TOWDATE)}
#' \item{DEPTH}{	A four digit number recording the average depth, to the nearest meter, during a survey gear deployment.(AVGDEPTH)}
#' \item{SURFTEMP}{Surface temperature of water (degrees Celcius).}
#' \item{BOTTEMP}{Bottom temperature (degrees Celsius).}
#' \item{SURFSALINITY}{Salinity at water surface in practical salinity units (PSU).}
#' \item{BOTSALINITY}{Bottom salinity in Practical Salinity Units (PSU).}
#' \item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#' \item{CATCHSEX}{Code used to identify species that are sexed at the catch level. See SVDBS.SEX_CODES}
#' \item{ABUNDANCE}{Expanded number of individuals of a species caught at a given station.(EXPCATCHNUM)}
#' \item{BIOMASS}{Expanded catch weight of a species caught at a given station. (EXPCATCHWT)}
#'
#'
#' The date:
#'
#' \item{pullDate}{The date the data was pulled from the database}
#'
#' The expression:
#'
#' \item{functionCall}{The call used to create the data pull}
#'
#' The version:
#'
#' \item{version}{The version of survdat used to pull the data}
#'
#'
#'@examples
#'\dontrun{
#'channel <- dbutils::connect_to_database("serverName","userName")
#'get_mass_inshore_survey_data(channel)
#'}
#'
#' @family survdat
#'
#'@export

get_mass_inshore_survey_data <- function(channel, filterByYear = NA) {
  call <- capture_function_call()
  version <- packageVersion("survdat")

  cruise.qry <- "select unique year, cruise6, svvessel, season
                 from svdbs.mstr_cruise
                 where purpose_code = 11
                 and year >= 1963
                 order by year, cruise6"

  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)

  #Use cruise codes to select other data
  cruise6 <- sqltext(cruise$CRUISE6)

  #Station data
  station.qry <- paste(
    "select unique cruise6, svvessel, station, stratum,
                        tow, decdeg_beglat as lat, decdeg_beglon as lon,
                        begin_est_towdate as est_towdate, avgdepth as depth,
                        surftemp, surfsalin, bottemp, botsalin
                        from Union_fscs_svsta
                        where cruise6 in (",
    cruise6,
    ")
                        and SHG <= 136
                        order by cruise6, station",
    sep = ''
  )

  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))

  #merge cruise and station
  survdat.mass <- merge(cruise, station)

  #Catch data
  catch.qry <- paste(
    "select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from UNION_FSCS_SVCAT
                     where cruise6 in (",
    cruise6,
    ")
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp",
    sep = ''
  )

  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))

  #merge with survdat
  data.table::setkey(survdat.mass, CRUISE6, STATION, STRATUM, TOW)
  survdat.mass <- merge(survdat.mass, catch, by = data.table::key(survdat.mass))

  #Convert number fields from chr to num
  numberCols <- c(
    'CRUISE6',
    'STATION',
    'STRATUM',
    'TOW',
    'SVSPP',
    'CATCHSEX',
    'YEAR'
  )
  survdat.mass[,
    (numberCols) := lapply(.SD, as.numeric),
    .SDcols = numberCols
  ][]

  if (all(!is.na(filterByYear))) {
    survdat.mass <- survdat.mass |>
      dplyr::filter(YEAR %in% filterByYear)
  }

  sql <- c(cruise.qry, station.qry, catch.qry)

  return(list(
    survdat = survdat.mass,
    sql = sql,
    pullDate = date(),
    functionCall = call,
    version = version
  ))
}
