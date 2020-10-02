#'Generate a table of stratum areas
#'
#'Calculates the area of a stratum from a shapefile.  Necessary if post stratifying
#'a survdat file.
#'
#'@family Survdat
#'
#'@param stratum sf object. Name of the object containing the shapefile.
#'@param col.name Character String. Column name from \code{stratum} that contains the strata designations.
#'@param crs Character string. Specifiy the coordinate refernce system.
#'
#'@return Returns a data.table (nx2).
#'
#'\item{STRATA}{The name of each Region}
#'\item{AREA}{The area of the STRATA in square kilometers}
#'
#'@section Coordinate reference system (CRS):
#'The deafult CRS is the Lambert Conformal Conic as is denoted by :
#'"+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "
#'
#'@importFrom magrittr "%>%"
#'
#'@export


get_area <- function(stratum, col.name, crs="+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "){

  # Find area of polygons based on coordinate reference system supplied
  Area <- units::set_units(sf::st_area(stratum,crs),km^2)

  strata <- stratum %>%
    as.data.frame() %>%
    dplyr::select(col.name) %>%
    dplyr::rename(STRATUM = col.name) %>%
    cbind(.,Area) %>%
    data.table::as.data.table()

  return(strata)
}
