# Generate a table of stratum areas

Calculates the area of a polygon from a shapefile.

## Usage

``` r
get_area(areaPolygon, areaDescription)
```

## Arguments

- areaPolygon:

  sf object. Name of the object containing the shapefile.

- areaDescription:

  Character String. Column name from `areaPolygon` that contains the
  strata designations.

## Value

Returns a data.table (nx2).

- STRATA:

  The name of each Region

- AREA:

  The area of the STRATA in square kilometers

## Coordinate reference system (CRS)

The deafult CRS is the Lambert Conformal Conic as is denoted by :

"+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0
+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "

## See also

Other survdat:
[`calc_stratified_mean()`](https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.md),
[`calc_swept_area()`](https://noaa-edab.github.io/survdat/reference/calc_swept_area.md),
[`get_mass_inshore_survey_data()`](https://noaa-edab.github.io/survdat/reference/get_mass_inshore_survey_data.md),
[`get_survdat_clam_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_clam_data.md),
[`get_survdat_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_data.md),
[`get_survdat_scallop_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_scallop_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#Find the area of each Stratum in the strata.shp shapefile (bundled with the package)
area <- sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
get_area(areaPolygon = area, areaDescription="STRATA")
} # }
```
