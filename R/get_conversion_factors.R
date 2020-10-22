#' Extract CONVERSION FACTORS from SVDBS
#'
#'Extract a list of conversion factors from the SVDBS SURVAN_CONVERSION_FACTORS table
#'
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'@section Reference:
#'Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete CONVERSION FACTORS table
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_conversion_factors(channel)
#'}
#'
#' @export

get_conversion_factors <- function(channel){

  # creates the sql based on user input
  sqlStatement <-   convert.qry <- "select * from SURVAN_CONVERSION_FACTORS"

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'SURVAN_CONVERSION_FACTORS' and owner='SVDBS';"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



