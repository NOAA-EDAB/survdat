# Extract Survey strata that comprise species stock definition

Extract a list of survey strata from STOCKEFF supporting table

## Usage

``` r
get_species_stock_area(channel, species = "all", stock_name = NULL)
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to communicate with the database engine. (see
  `connect_to_database`)

- species:

  A specific species code or set of codes. Either numeric or character
  vector. Defaults to "all" species. Numeric codes (SPECIES_ITIS) are
  converted to VARCHAR2 (6) when creating the sql statement.

- stock_name:

  Character string. Upper or lower case. The abbreviated name of the
  stock (default = NULL, pulls all stocks). For Example "GBK", "EGOM"

## Value

A list is returned:

- data:

  containing the result of the executed `sqlStatement`

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

The default sql statement
"`select * from STOCKEFF.V_SV_STOCK_RECENT_STRATA_O`" is used

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other helper:
[`get_conversion_factors()`](https://noaa-edab.github.io/survdat/reference/get_conversion_factors.md),
[`get_cruise_purpose()`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose.md),
[`get_length_weight()`](https://noaa-edab.github.io/survdat/reference/get_length_weight.md),
[`get_maturity()`](https://noaa-edab.github.io/survdat/reference/get_maturity.md),
[`get_sex()`](https://noaa-edab.github.io/survdat/reference/get_sex.md),
[`get_sex_fscs()`](https://noaa-edab.github.io/survdat/reference/get_sex_fscs.md),
[`get_species()`](https://noaa-edab.github.io/survdat/reference/get_species.md),
[`get_strata()`](https://noaa-edab.github.io/survdat/reference/get_strata.md),
[`get_vessel()`](https://noaa-edab.github.io/survdat/reference/get_vessel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete area table based on default sql statement
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_species_stock_area(channel)

# extracts info for cod (164712)
get_species_stock_area(channel,species=164712)

# extracts info for cod ("COD"). All stocks
get_species_stock_area(channel,"cod")
get_species_stock_area(channel,"co")
get_species_stock_area(channel,"COD")

# extracts info for cod (GBK stock)
get_species_stock_area(channel,"COD", stock_name = "GBK")
get_species_stock_area(channel,"CO", stock_name = "gbk")
get_species_stock_area(channel,"164712", stock_name = "WGOM")
get_species_stock_area(channel,164712, stock_name = "wgom")

# extracts info for cod (164712)  and bluefish (168559)
sqlStatement <- "select * from cfdbs.species_itis_ne"
get_species_stock_area(channel,species= c("164712","168559"))

} # }
```
