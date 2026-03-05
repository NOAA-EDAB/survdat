# Extract species specific LENGTH-WEIGHT COEFFICIENTS from SVDBS

Pulls the length-weight coefficients from
SVDBS.LENGTH_WEIGHT_COEFFICIENTS table These coefficients are described
in NOAA Tech Memo NMFS-NE-171.

## Usage

``` r
get_length_weight(channel)
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to communicate with the database engine. (see
  `connect_to_database`)

## Value

Returns a data.table (nx12).

- SVSPP:

  A standard code which represents a species caught in a trawl or
  dredge. Refer to the SVDBS.SVSPECIES_LIST

- SVLWEXP:

  The exponent of the length-weight equation, b.

- SVLWCOEFF:

  The natural log of the coefficient of the length-weight equation, ln
  a.

## See also

Other helper:
[`get_conversion_factors()`](https://noaa-edab.github.io/survdat/reference/get_conversion_factors.md),
[`get_cruise_purpose()`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose.md),
[`get_maturity()`](https://noaa-edab.github.io/survdat/reference/get_maturity.md),
[`get_sex()`](https://noaa-edab.github.io/survdat/reference/get_sex.md),
[`get_sex_fscs()`](https://noaa-edab.github.io/survdat/reference/get_sex_fscs.md),
[`get_species()`](https://noaa-edab.github.io/survdat/reference/get_species.md),
[`get_species_stock_area()`](https://noaa-edab.github.io/survdat/reference/get_species_stock_area.md),
[`get_strata()`](https://noaa-edab.github.io/survdat/reference/get_strata.md),
[`get_vessel()`](https://noaa-edab.github.io/survdat/reference/get_vessel.md)
