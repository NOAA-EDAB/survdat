#' Extract MATURITY information from SVDBS
#'
#'Extract a list of maturity codes and descriptions from the SVDBS FSCS_MATURITY_CODES table
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
#' # extracts complete maturity table
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_maturity(channel)
#'}
#'
#' @export

get_maturity <- function(channel){

  # creates the sql based on user input
  sqlStatement <- "select maturity, old_maturity, maturity_description from SVDBS.FSCS_MATURITY_CODES"

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'FSCS_MATURITY_CODES' and owner='SVDBS';"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



