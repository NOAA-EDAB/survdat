#' Extract SEX information from SVDBS
#'
#'Extract a list of sex codes and descriptions from the SVDBS FSCS_SEX_CODES table
#'
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'
#' @return A list is returned:
#'
#'   \item{data}{tibble containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'@section Reference:
#'Use the data dictionary for field name explanations
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#'@family helper
#'
#' @examples
#' \dontrun{
#' # extracts complete sex table
#' channel <- dbutils::connect_to_database(server="serverName",uid="userName")
#' get_sex_fscs(channel)
#'}
#'
#' @export

get_sex_fscs <- function(channel){

  # creates the sql based on user input
  sqlStatement <- "select sex, sex_description from SVDBS.FSCS_SEX_CODES"

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'FSCS_SEX_CODES' and owner='SVDBS';"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



