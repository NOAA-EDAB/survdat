# Plots shapefile

Plots shapefile

## Usage

``` r
plot_shapefile(polygons, crs = 4269, filterPolygons = NULL)
```

## Arguments

- polygons:

  sf object. The polygons (shapefile)

- crs:

  Character string. Defines the coordinate reference system for
  projection. Default = 4269 (NAD83)

- filterPolygons:

  Numeric Vector. (Default = NULL) Set of polygons to plot in a differnt
  color

## Value

A figure

## See also

Other plotting:
[`plot_data_area()`](https://noaa-edab.github.io/survdat/reference/plot_data_area.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Read in shapefile
area <- sf::st_read(dsn = system.file("extdata","strata.shp",package="survdat"),quiet=T)
plot_shapefile(polygons=area)

# Plots shapefile and highlights GB region
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
plot_shapefile(polygons=area,filterPolygons = GB)

} # }
```
