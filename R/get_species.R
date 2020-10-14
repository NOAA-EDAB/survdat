#' Extract SPECIES information from SVDBS
#'
#'Extract a list of speices names, code, market category, etc from the SVDBS SVSPECIES_LIST table
#'
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#' @param species a specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes are converted to VARCHAR2(3 BYTE) when creating the sql statement. Character codes are short character strings.
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
#'Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations.
#'Note: species codes (svspp) are stored in the database as VARCHAR2(3 BYTE)
#'
#' @seealso \code{\link{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts complete species table based on custom sql statement
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_species(channel)
#'
#' # extracts info for cod (73)
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_species(channel,species=73)
#'
#' # extracts info for cod ("COD")
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_species(channel,"cod") or
#' get_species(channel,"co") or
#' get_species(channel,"COD")
#'
#'
#' # extracts info for cod (73)  and bluefish (135)
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' sqlStatement <- "select * from cfdbs.cfspp"
#' get_species(channel,species= c("73","135"))
#'}
#'
#' @export

get_species <- function(channel,species="all"){

  # creates the sql based on user input
  sqlStatement <- dbutils::create_sql(species,fieldName="svspp",fieldName2="comname",dataType="%03d",defaultSqlStatement="select * from svdbs.svspecies_list")

  query <- DBI::dbGetQuery(channel,sqlStatement)

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'SVSPECIES_LIST' and owner='SVDBS';"
  colNames <- t(DBI::dbGetQuery(channel,sqlcolName))

  return (list(data=dplyr::as_tibble(query),sql=sqlStatement, colNames=colNames))

}



