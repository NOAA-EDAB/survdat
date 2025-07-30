#' Extract Survey strata that comprise species stock definition
#'
#'Extract a list of survey strata from STOCKEFF supporting table
#'
#'
#' @inheritParams get_survdat_data
#' @param species A specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes (SPECIES_ITIS) are converted to VARCHAR2 (6) when creating the sql statement.
#' @param stock_name Character string. Upper or lower case. The abbreviated name of the stock (default = NULL, pulls all stocks). For Example "GBK", "EGOM"
#' @return A list is returned:
#'
#'    \item{data}{containing the result of the executed \code{sqlStatement}}
#'
#'    \item{sql}{containing the sql call}
#'
#'    \item{colNames}{ a vector of the table's column names}
#'
#'The default sql statement "\code{select * from STOCKEFF.V_SV_STOCK_RECENT_STRATA_O}" is used
#'
#'@section Reference:
#'Use the data dictionary for field name explanations
#'
#'@family helper
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete area table based on default sql statement
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_species_stock_area(channel)
#'
#' # extracts info for cod (164712)
#' get_species_stock_area(channel,species=164712)
#'
#' # extracts info for cod ("COD"). All stocks
#' get_species_stock_area(channel,"cod")
#' get_species_stock_area(channel,"co")
#' get_species_stock_area(channel,"COD")
#'
#' # extracts info for cod (GBK stock)
#' get_species_stock_area(channel,"COD", stock_name = "GBK")
#' get_species_stock_area(channel,"CO", stock_name = "gbk")
#' get_species_stock_area(channel,"164712", stock_name = "WGOM")
#' get_species_stock_area(channel,164712, stock_name = "wgom")
#'
#' # extracts info for cod (164712)  and bluefish (168559)
#' sqlStatement <- "select * from cfdbs.species_itis_ne"
#' get_species_stock_area(channel,species= c("164712","168559"))
#'
#'}
#'
#' @export
#'
#
get_species_stock_area <- function(channel, species = "all", stock_name = NULL){

  nameType <-  "COMMON_NAME"
  # creates the sql based on user input
  sqlStatement <- dbutils::create_sql(species,fieldName="species_itis",fieldName2=nameType,dataType="%06d",defaultSqlStatement="select * from STOCKEFF.V_SV_STOCK_RECENT_STRATA_O")

  # strip ; and add additional content
  sqlStatement <- sub(";","",sqlStatement)
  if (!is.null(stock_name)) {
    stock_name <- toupper(stock_name)
    sqlStatement <- paste0(sqlStatement, " and STOCK_ABBREV = '", stock_name, "'")
  }

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'V_CF_STOCK_RECENT_STAT_AREA_O' and owner='STOCKEFF'"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))
}
