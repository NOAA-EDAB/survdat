#'Post stratify NEFSC survey data
#'
#'Uses a shapefile to post stratify survey data collected
#'during the NEFSC surveys
#'
#'@family Survdat
#'
#'@param survdat NEFSC survey data generated by Survdat.R.
#'@param stratum R object containing the shapefile.
#'@param strata.col Column name from stratum that contains the strata designations.
#'@param crs Character string. Specifiy the coordinate refernce system.
#'@param na.keep Logical value to indicate whether original strata names should be retained.
#'
#'@return Returns a survdat object with new strata designations labeled as \code{newstrata}.
#'
#'@importFrom magrittr "%>%"
#'
#'@export


post_strat <- function (survdata, stratum, strata.col = 'EPU', crs = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0" ,na.keep = F) {

  # transform data pull to required crs
  #survdata <- sf::st_as_sf(x=survdata,coords = c("LON","LAT"),crs = crs)
  # transform Regional Shape file to same crs
  areas <- stratum %>%
    dplyr::rename(strata.col = strata.col) %>%
    sf::st_transform(.,crs)


  stations <- survdata %>%
    dplyr::select(CRUISE6, STRATUM, STATION, LAT, LON) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(.,coords = c("LON","LAT"),crs=4326) %>%
    sf::st_transform(.,crs)


  # intersect the stations with the polygon
  station_area <- sf::st_join(stations,areas,join = sf::st_intersects) %>%
    dplyr::select(names(stations),strata.col) %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(CRUISE6,STRATUM,STATION)

  # join data with stations (which nw are assigned to an area based on the shape file)
  master <- base::merge(survdata,station_area,by = c("CRUISE6","STRATUM","STATION")) %>%
    dplyr::rename(!!strata.col := strata.col)


  if (!(na.keep)) { # removes all points that fall outside of the areas defined by the polygons in stratum
    master <- master %>%
      dplyr::filter(!is.na(get(strata.col)))
  }


  return(master)

}
