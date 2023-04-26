#' Plots shapefile
#'
#'@param polygons sf object. The polygons (shapefile)
#'@param crs Character string. Defines the coordinate reference system for projection. Default = 4269 (NAD83)
#'@param filterPolygons Numeric Vector. (Default = NULL) Set of polygons to plot in a differnt color
#'
#'@return A figure
#'
#'@family plotting
#'
#'@examples
#'\dontrun{
#'
#'# Read in shapefile
#'area <- sf::st_read(dsn = system.file("extdata","strata.shp",package="survdat"),quiet=T)
#'plot_shapefile(polygons=area)
#'
#'# Plots shapefile and highlights GB region
#'GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
#'plot_shapefile(polygons=area,filterPolygons = GB)
#'
#'}
#'
#'@export

plot_shapefile <- function(polygons,crs=4269, filterPolygons=NULL) {

  # transform crs
  polygons <- sf::st_transform(polygons,crs)


  if (is.null(filterPolygons)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data=polygons,color = "Grey",alpha = .5)

  } else {

    # plot shapefile
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data=polygons,color = "Grey",alpha = .5) +
      ggplot2::geom_sf(data=polygons %>% dplyr::filter(STRATA %in% filterPolygons),
                       color = "blue",alpha = .5)
  }

  print(p)
  #return(p)

}
