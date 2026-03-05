# Extract STRATA information from SVDBS

Extract STRATA information from the SVDBS.SVMSTRATA table

## Usage

``` r
get_strata(channel, strata = "all")
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to connect to communicate with the database
  engine. (see `connect_to_database`)

- strata:

  Numeric or character vector. Set of strata codes. Default = "all".
  Numeric codes are converted to VARCHAR2(5 BYTE) when creating the sql
  statement. Alternatively enter name of stratum

## Value

A list is returned:

- species:

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
[`get_maturity()`](https://noaa-edab.github.io/survdat/reference/get_maturity.md),
[`get_sex()`](https://noaa-edab.github.io/survdat/reference/get_sex.md),
[`get_sex_fscs()`](https://noaa-edab.github.io/survdat/reference/get_sex_fscs.md),
[`get_species()`](https://noaa-edab.github.io/survdat/reference/get_species.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/survdat/reference/get_species_stock_area.md),
[`get_vessel()`](https://noaa-edab.github.io/survdat/reference/get_vessel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete strata table
channel <- dbutils::connect_to_database(server="serverName",uid="userName")
get_strata(channel)

# extracts info for a single stratum
get_strata(channel,strata=6940)
get_strata(channel,strata="6940")
get_strata(channel,"SNE BLOCK ISLAND")
get_strata(channel,"SNE Block Island")

# extracts multiple strata
get_strata(channel,strata= c(6940,6400,6380))
get_strata(channel,strata=c("6940","6400","6380"))
} # }
```
