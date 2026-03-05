# Extract SPECIES information from SVDBS

Extract a list of speices names, code, market category, etc from the
SVDBS SVSPECIES_LIST table

## Usage

``` r
get_species(channel, species = "all")
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to connect to communicate with the database
  engine. (see `connect_to_database`)

- species:

  Numeric or character vector. A specific species code or set of codes.
  Default = "all". Numeric codes are converted to VARCHAR2(3 BYTE) when
  creating the sql statement. Character codes are short character
  strings.

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

Note: species codes (svspp) are stored in the database as VARCHAR2(3
BYTE)

## See also

`connect_to_database`

Other helper:
[`get_conversion_factors()`](https://noaa-edab.github.io/survdat/reference/get_conversion_factors.md),
[`get_cruise_purpose()`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose.md),
[`get_length_weight()`](https://noaa-edab.github.io/survdat/reference/get_length_weight.md),
[`get_maturity()`](https://noaa-edab.github.io/survdat/reference/get_maturity.md),
[`get_sex()`](https://noaa-edab.github.io/survdat/reference/get_sex.md),
[`get_sex_fscs()`](https://noaa-edab.github.io/survdat/reference/get_sex_fscs.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/survdat/reference/get_species_stock_area.md),
[`get_strata()`](https://noaa-edab.github.io/survdat/reference/get_strata.md),
[`get_vessel()`](https://noaa-edab.github.io/survdat/reference/get_vessel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# extracts complete species table based on custom sql statement
channel <- dbutils::connect_to_database(server="serverName",uid="userName")
get_species(channel)

# extracts info for cod (73)
get_species(channel,species=73)
get_species(channel,"cod")
get_species(channel,"co")
get_species(channel,"COD")

# extracts info for cod (73)  and bluefish (135)
get_species(channel,species= c("73","135"))
} # }
```
