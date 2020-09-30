#' Plots survey data and shapefile
#'
#'@param points sf object. The data points
#'@param polygons sf object. The polygons (shapefile)
#'@param crs Character string. Defines the coordinate reference system for projection
#'
#'@return A figure
#'
#'@export

plot_data_area <- function(points,polygons,crs) {

  # Make sure both point and polygons are on the same projection
  sf::st_transform(points,crs)
  sf::st_transform(polygons,crs)


  ggplot2::ggplot() +
    ggplot2::geom_sf(data=points,size=1,alpha=0.2) +
    ggplot2::geom_sf(data=polygons,color = "Blue",alpha = .5)


}
