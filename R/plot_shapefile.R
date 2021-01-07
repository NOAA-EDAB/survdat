#' Plots shapefile
#'
#'@param polygons sf object. The polygons (shapefile)
#'@param crs Character string. Defines the coordinate reference system for projection. Default = 4269 (NAD83)
#'
#'@return A figure
#'
#'@export

plot_shapefile <- function(polygons,crs=4269) {

  # transform crs
  sf::st_transform(polygons,crs)

  # plot shapefile
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data=polygons,color = "Grey",alpha = .5)

  print(p)
  #return(p)

}
