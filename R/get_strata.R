#' Extract STRATA information from SVDBS
#'
#'Extract STRATA information from the SVDBS SVMSTRATA table
#'
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#' @param strata Numeric or character vector. Set of strata codes. Defaults to "all" strata.
#' Numeric codes are converted to VARCHAR2(5 BYTE) when creating the sql statement. Alternatively enter name of stratum
#'
#' @return A list is returned:
#'
#'   \item{species}{containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'The default sql statement "\code{select * from svdbs.svspecies_list}" is used
#'
#'@section Reference:
#'Use the data dictionary for field name explanations
#'
#'
#' @seealso \code{\link[dbutils]{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete strata table
#' channel <- dbutils::connect_to_database(server="name_of_server",uid="individuals_username")
#' get_strata(channel)
#'
#' # extracts info for a single stratum
#' get_strata(channel,strata=6940)
#' get_strata(channel,strata="6940")
#' get_strata(channel,"SNE BLOCK ISLAND")
#' get_strata(channel,"SNE Block Island")
#'
#' # extracts multiple strata
#' get_strata(channel,strata= c(6940,6400,6380))
#' get_strata(channel,strata=c("6940","6400","6380"))
#' }
#'
#' @export

get_strata <- function(channel,strata="all"){

  # creates the sql based on user input
  sqlStatement <- dbutils::create_sql(strata,fieldName="stratum",fieldName2="stratum_name",dataType="%05d",defaultSqlStatement="select * from SVDBS.SVMSTRATA")

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'SVMSTRATA' and owner='SVDBS';"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



