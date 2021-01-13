#' Assigns points to polygon
#'
#' Assign survey data (points, lat and lon) to designated regions (polygons) from a shape file.
#'
#'
#' @inheritParams strat_prep
#' @param na.keep Boolean. Logical value to indicate whether original strata names
#'  should be retained.
#'
#' @return Returns a \code{surveyData} data.table with one additional column labeled
#'  with the value of \code{areaDescription}
#'
#' \item{areaDescription}{The name of the region (found in \code{areaPolygon})
#'  that a record in \code{surveyData} is assigned to}
#'
#' @importFrom magrittr "%>%"
#'
#'@family survdat
#'
#' @export


post_strat <- function (surveyData, areaPolygon, areaDescription, na.keep = F) {

  # transform Regional Shape file using lambert conformal conic coordinate ref system
  crs <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  areas <- areaPolygon %>%
    dplyr::rename(areaDescription = areaDescription) %>%
    sf::st_transform(., crs)

  # find unique stations and transform to required crs
    stations <- surveyData %>%
    dplyr::select(CRUISE6, STRATUM, STATION, LAT, LON) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(., coords = c("LON","LAT"), crs=4326) %>%
    sf::st_transform(., crs)


  # Intersect the stations with the polygon
  # Assigns stations with polygons
  station_area <- sf::st_join(stations, areas, join = sf::st_intersects) %>%
    dplyr::select(names(stations), areaDescription) %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(CRUISE6, STRATUM, STATION)

  # Join survey data with stations (which now are assigned to an area based on the shape file)
  master <- base::merge(surveyData, station_area,
                        by = c("CRUISE6","STRATUM","STATION")) %>%
    dplyr::rename(!!areaDescription := areaDescription)

  # check to see if we want to keep points that fall outside of all o fthe polygons found in the shape file
  if (!(na.keep)) { # removes all points that fall outside of the areas defined by the polygons in stratum
    master <- master %>%
      dplyr::filter(!is.na(get(areaDescription))) %>%
      data.table::as.data.table()
  }


  return(master)

}

