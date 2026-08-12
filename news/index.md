# Changelog

## survdat 2.0.0

### Breaking changes

- Dependency on R 4.1.0 to accommodate the native pipe
- Following functions, previously internal, are now external functions
  available to the user
  - `post_strat`
  - `strat_mean`
  - `strat_prep`
  - `swept_area`

### New features

- Printing of intermediate function output has been removed from
  [`calc_stratified_mean()`](https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.md)

### Patch fixes

- Units of variables in
  [`calc_stratified_mean()`](https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.md)
  correctly represent data
- `get_clam_survey_data()` reworked to use updated strata definitions
  from 2018 and resolved issue of dropped data
- `lwgeom` no longer utilized in area calculation
- Typecasting errors when converting between data.table and tibble

## survdat 1.2.0

### New features

- `get_mass_inshore_survey_data` - pulls data from the Massachusetts
  Inshore Survey

## survdat 1.1.1

### Patch fixes

- `calc_swept_area` function can now use a user supplied value of `a`
  (average swept area of trawl, km^-2)
- `swep_area` function can now utilize a user supplied scalar for `q`
  (catchability) across all groups

## survdat 1.1.0

- Added `get_species_stock_area` function to retrieve species stock area
  (Bottom Trawl survey STRATA) data from STOCKEFF

## survdat 1.0.1

Representative tows are now being pulled differently. No longer using
TOGA \<= 1324. Replaced by: \* tow_code \<= 1 \* operation_code \<= 3 \*
gear_code \<= 2

## survdat 1.0

Benchmark release
