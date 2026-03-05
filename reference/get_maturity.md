# Extract MATURITY information from SVDBS

Extract a list of maturity codes and descriptions from the SVDBS
FSCS_MATURITY_CODES table

## Usage

``` r
get_maturity(channel)
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to connect to communicate with the database
  engine. (see `connect_to_database`)

## Value

A list is returned:

- data:

  containing the result of the executed `$sql` statement

- sql:

  containing the sql call

- colNames:

  a vector of the table's column names

## Reference

Use the data dictionary for field name explanations

## See also

`connect_to_database`

Other helper:
[`get_conversion_factors()`](https://noaa-edab.github.io/survdat/reference/get_conversion_factors.md),
[`get_cruise_purpose()`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose.md),
[`get_length_weight()`](https://noaa-edab.github.io/survdat/reference/get_length_weight.md),
[`get_sex()`](https://noaa-edab.github.io/survdat/reference/get_sex.md),
[`get_sex_fscs()`](https://noaa-edab.github.io/survdat/reference/get_sex_fscs.md),
[`get_species()`](https://noaa-edab.github.io/survdat/reference/get_species.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/survdat/reference/get_species_stock_area.md),
[`get_strata()`](https://noaa-edab.github.io/survdat/reference/get_strata.md),
[`get_vessel()`](https://noaa-edab.github.io/survdat/reference/get_vessel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete maturity table
channel <- dbutils::connect_to_database(server="serverName",uid="userName")
get_maturity(channel)
} # }
```
