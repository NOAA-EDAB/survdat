#' Extract Length and weight data from SVDBS
#'
#'Extract a list of length and weights for species sampled in nefsc surveys (spring and fall)
#'This data is extracted from svdbs.union_fscs_svbio
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}.
#' This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param year Numeric or character vector. The Year(s) to pull data for. (Uses the cruise6 field to search for year). Default = 1994. For all years use year = "all"
#' @param species Numeric vector. Survey code for species (SVSPP codes). Defaults to "all" species.
#' Numeric codes are converted to VARCHAR2(3 BYTE) when creating the sql statement.
#' @param sex Character vector. Default = "all". options "M" (male), "F" (female), "U" (unsexed)
#'
#' @return A list of 3 items \code{data}, \code{sql}, \code{colNames}
#'
#' The data frame (\code{data}) (Descriptions taken from NEFSC Data dictionary)
#'
#' \item{CRUISE6}{Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year. The 5th byte signifies cruises other than groundfish: Shrimp survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e. 201491), Food habits = 5 (i.e.199554)}
#' \item{STRATUM}{	A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes). Stratum group refers to if area fished is inshore or offshore North or South of Cape Hatteras or the type of cruise (shellfish, State of MA, offshore deepwater). The stratum number (third and fourth digits of code) refers to area defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the code increases the length of the stratum number for revised strata after the Hague Line was established. Stratum group code: 01 = Trawl, offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl, inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09 = MA DMF; 99 = Offshore deepwater (outside the stratified area). A change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has been in effect since Spring 1987, and may be summarized as follows: Previous strata: 01350; Present strata: 01351, 01352.}
#' \item{TOW}{Sequential number representing order in which station was selected within a stratum.}
#' \item{STATION}{Unique sequential order in which stations have been completed. Hangups and short tows each receive a non-repeated consecutive number.}
#' \item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#' \item{SEASON}{	Season of the year in which cruise was conducted.}
#' \item{SEX}{Code indicating sex of fish or invertebrate species. See SVDBS.FSCS_SEX_CODES if using fscs data and SVDBS.SEX_CODES if using non FSCS data. Codes 0, 1, 2 and 4 are the only valid codes in fscs tables.}
#' \item{INDID}{A unique identifier for each fish sampled.}
#' \item{LENGTH}{Measured length of species in centimeters (cm). Measure method differs by species.}
#' \item{INDWT}{Individual weight (KG) of species being sampled.}
#' \item{STOM_WGT}{Stomach weight of an individual fish in grams.}
#' \item{STOM_VOLUME}{Volume of the stomach contents of the fish sampled, measured to the nearest tenth of a cubic centimeter (cc).}
#' \item{MATURITY}{Stage of maturation of the fish being sampled. See SVDBS.FSCS_MATURITY_CODES}
#'
#' The sql statement
#'
#' \item{sql}{containing the sql call}
#'
#' The data frame listing all columns in the table
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'@section Reference:
#'
#' Note: species codes (SVSPP) are stored in the database as VARCHAR2(3 BYTE)
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts length-weight data for cod (73) for all years and sexes
#' channel <- connect_to_database(server="servername",uid="username")
#' codPull <- get_length_weight_data(channel,year = "all", species=73)
#'}
#'
#' @export


get_length_weight_data <- function(channel, year=1994, species="all", sex="all"){

  if((length(year) == 1) & (length(species) == 1)) {
    if ((year == "all") & (species == "all")) stop("Can not pull all species and all years. Too much data!!")
  }
  # create an SQL query to extract all relavent data from tables
  # list of strings to build where clause in sql statement
  whereVec <- list()

  whereVec[[1]] <-  dbutils::createString(itemName="m.svspp",species,convertToCharacter=TRUE,numChars=3)
  whereVec[[3]] <-  dbutils::createStringYear(itemName="m.cruise6",year,convertToCharacter=TRUE,numChars=4)

  # sex conversion
  if (tolower(sex) == "all") {
    sex <- c(0,1,2)
  } else if (!is.numeric(sex)) {
    sex <- gsub("M",1,sex)
    sex <- gsub("F",2,sex)
    sex <- as.numeric(gsub("U",0,sex))
  }

  whereVec[[4]] <-  paste("m.sex in (",toString(sex),")")

  # build where clause of SQL statement based on input above
  whereStr <- "where"
  for (item in whereVec) {
    if (is.null(item)) {next}
    if (which(item == whereVec) == length(whereVec)) {
      whereStr <- paste(whereStr,item)
      next
    }
    whereStr <- paste(whereStr,item,"and")
  }

  # eventually user will be able to pass these variables
  sqlStatement <- "select m.cruise6, m.stratum, m.tow, m.station, m.svspp, m.sex, m.indid, m.length, m.indwt, m.maturity, m.stom_wgt, m.stom_volume, s.season
                    from svdbs.union_fscs_svbio m LEFT JOIN svdbs.svdbs_cruises s
                    ON m.cruise6 = s.cruise6 "

  sqlStatement <- paste(sqlStatement,whereStr)

  # call database
  query <- DBI::dbGetQuery(channel,sqlStatement)

  # column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'UNION_FSCS_SVBIO' and owner='SVDBS'"
  colNames <- DBI::dbGetQuery(channel,sqlcolName)

  query <- dplyr::as_tibble(query)
  query$SEASON <- as.factor(query$SEASON)
  query$LENGTH <- as.numeric(query$LENGTH)
  query$INDWT <- as.numeric(query$INDWT)
  query$SEX <- as.integer(query$SEX)


  return (list(data =query,sql=sqlStatement, colNames=colNames))
}



