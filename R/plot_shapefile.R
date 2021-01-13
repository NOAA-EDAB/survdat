#' Plots shapefile
#'
#'@param polygons sf object. The polygons (shapefile)
#'@param crs Character string. Defines the coordinate reference system for projection. Default = 4269 (NAD83)
#'
#'@return A figure
#'
#'#'@family plotting
#'
#'@examples
#'\dontrun{
#'
#'# Read in shapefile
#'area <- sf::st_read(dsn = system.file("extdata","strata.shp",package="survdat"),quiet=T)
#'plot_shapefile(polygons=area)
#'}
#'
#'@export

plot_shapefile <- function(polygons,crs=4269) {

  # transform crs
  polygons <- sf::st_transform(polygons,crs)

  # plot shapefile
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data=polygons,color = "Grey",alpha = .5)

  print(p)
  #return(p)

}
