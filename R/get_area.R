#'Generate a table of stratum areas
#'
#'Calculates the area of a polygon from a shapefile.
#'
#'
#'@param areaPolygon sf object. Name of the object containing the shapefile.
#'@param areaDescription Character String. Column name from \code{areaPolygon}
#'                       that contains the strata designations.
#'
#'@return Returns a data.table (nx2).
#'
#'\item{STRATA}{The name of each Region}
#'\item{AREA}{The area of the STRATA in square kilometers}
#'
#'@section Coordinate reference system (CRS):
#'The deafult CRS is the Lambert Conformal Conic as is denoted by :
#'
#'"+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "
#'
#'@importFrom magrittr "%>%"
#'
#'@family survdat
#'
#'@examples
#'\dontrun{
#' #Find the area of each Stratum in the strata.shp shapefile (bundled with the package)
#' area <- sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
#' get_area(areaPolygon = area, areaDescription="STRATA")
#'}
#'
#'@export


get_area <- function(areaPolygon, areaDescription){

  # Find area of polygons based on a lambert conformal conic coordinate reference
  # system
  crs = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  Area <- units::set_units(sf::st_area(areaPolygon, crs),km^2)

  strata <- areaPolygon %>%
    as.data.frame() %>%
    dplyr::select(areaDescription) %>%
    dplyr::rename(STRATUM = areaDescription) %>%
    cbind(.,Area) %>%
    data.table::as.data.table()

  return(strata)
}
