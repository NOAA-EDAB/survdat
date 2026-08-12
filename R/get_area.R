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
#'The deafult CRS is the Albers Equal Area as is denoted by :
#'
#'"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "
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

get_area <- function(areaPolygon, areaDescription) {
  # Find area of polygons based on a lambert conformal conic coordinate reference
  # system
  #crs <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # original custom CRS (+proj=lcc) was Conformal (it preserves shapes, but
  # distorts area). By changing it to +proj=aea (Equal-Area)
  # while keeping the exact same latitude/longitude center points,
  # the flat 2D math calculated by sf::st_area() will now match
  # the true ellipsoidal lwgeom values
  crs <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  # turn off spherical geometry. Causes an issue in st_area function
  sf::sf_use_s2(FALSE)
  # Repair any invalid geometries (resolves the duplicate vertex error)
  areaPolygon_clean <- sf::st_make_valid(areaPolygon)
  area_projected <- sf::st_transform(areaPolygon_clean, crs)

  # bypass units package check and assign km^2 manually
  raw_meters <- as.numeric(sf::st_area(area_projected))
  Area <- units::set_units(raw_meters / 1000000, "km^2")

  strata <- areaPolygon_clean %>%
    as.data.frame() %>%
    dplyr::select(areaDescription) %>%
    dplyr::rename(STRATUM = areaDescription) %>%
    cbind(., Area) %>%
    dplyr::group_by(STRATUM) %>%
    dplyr::summarise(sum(Area), .groups = 'keep') %>%
    dplyr::rename(Area = 'sum(Area)') %>%
    data.table::as.data.table()

  return(strata)
}
