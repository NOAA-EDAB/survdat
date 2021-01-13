#' Plots survey data and shapefile
#'
#' Quick plot of any shape file overlaid with lat, lon data.
#'
#'@param points Data frame. Survdat data points (Must include "LAT" and "LON" fields)
#'@param polygons sf object. The polygons (shapefile)
#'@param crs Character string. Defines the coordinate reference system for projection
#'
#'@return A figure
#'
#'@family plotting
#'
#'@examples
#'\dontrun{
#'
#'# Plot 2019 bottomline survey data with EPU regions
#'# Read in shapefile
#' area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
#'
#'# Get data
#' data <- get_survdat_Data(channel)
#' # Filter 2019 data
#' filteredData <- data$survdat %>% dplyr::filter(YEAR == 2019)
#' plot_data_area(points= filteredData,polygons = area)
#'
#'}
#'@export

plot_data_area <- function(points,polygons,crs=4269) {

  message("Please be patient. This may take a minute (or two) ...")
  # convert data points to sf
  points <- sf::st_as_sf(points,coords=c("LON","LAT")) %>%
    sf::st_set_crs(.,crs) %>%
    sf::st_transform(.,crs)

  # Make sure both point and polygons are on the same projection
  polygons <- sf::st_transform(polygons,crs)


  ggplot2::ggplot() +
    ggplot2::geom_sf(data=points,size=1,alpha=0.2) +
    ggplot2::geom_sf(data=polygons,color = "Blue",alpha = .5)


}
