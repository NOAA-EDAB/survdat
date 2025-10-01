# survdat 1.2.0

## New Feature

* `get_mass_inshore_survey_data` - pulls data from the Massachusetts Inshore Survey

# survdat 1.1.1

## Bug fixes

* `calc_swept_area` function can now use a user supplied value of `a` (average swept area of trawl, km^-2)
* `swep_area` function can now utilize a user supplied scalar for `q` (catchability) across all groups

# survdat 1.1.0

* Added `get_species_stock_area` function to retrieve species stock area (Bottom Trawl survey STRATA) data from STOCKEFF

# survdat 1.0.1

Representative tows are now being pulled differently. 
No longer using TOGA <= 1324. Replaced by:
  * tow_code <= 1
  * operation_code <= 3
  * gear_code <= 2

# survdat 1.0

Benchmark release 



