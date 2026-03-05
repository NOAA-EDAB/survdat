# Plots survey data and shapefile

Quick plot of any shape file overlaid with lat, lon data.

## Usage

``` r
plot_data_area(points, polygons, crs = 4269)
```

## Arguments

- points:

  Data frame. Survdat data points (Must include "LAT" and "LON" fields)

- polygons:

  sf object. The polygons (shapefile)

- crs:

  Character string. Defines the coordinate reference system for
  projection

## Value

A figure

## See also

Other plotting:
[`plot_shapefile()`](https://noaa-edab.github.io/survdat/reference/plot_shapefile.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Plot 2019 bottomline survey data with EPU regions
# Read in shapefile
area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)

# Get data
data <- get_survdat_Data(channel)
# Filter 2019 data
filteredData <- data$survdat %>% dplyr::filter(YEAR == 2019)
plot_data_area(points= filteredData,polygons = area)

} # }
```
