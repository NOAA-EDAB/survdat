# Extracts Massachusetts inshore bottom trawl survey data

Pulls survey data

## Usage

``` r
get_mass_inshore_survey_data(channel, filterByYear = NA)
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to communicate with the database engine. (see
  `connect_to_database`)

- filterByYear:

  Numeric vector. Subset of years from which to pull data. If not
  specified then all years are pulled. (Default = NA)

## Value

A list containing a Data frame (data.table) (n x 19), a list of SQL
queries used to pull the data, the date of the pull, and the call
expression. Each row of the data.table represents the catch of a species
on a specific tow along with physical attributes of the tow.

The data frame (Descriptions taken from NEFSC Data dictionary)

- CRUISE6:

  Code uniquely identifying cruise. The first four digits indicate the
  year and the last two digit uniquely identify the cruise within the
  year. The 5th byte signifies cruises other than groundfish: Shrimp
  survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e.
  201491), Food habits = 5 (i.e.199554)

- STATION:

  Unique sequential order in which stations have been completed. Hangups
  and short tows each receive a non-repeated consecutive number.

- STRATUM:

  A predefined area where a net dredge, or other piece of gear was
  deployed. Code consists of 2 parts: Stratum group code number (2
  bytes) and stratum number (3 bytes). Stratum group refers to if area
  fished is inshore or offshore North or South of Cape Hatteras or the
  type of cruise (shellfish, State of MA, offshore deepwater). The
  stratum number (third and fourth digits of code) refers to area
  defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the
  code increases the length of the stratum number for revised strata
  after the Hague Line was established. Stratum group code: 01 = Trawl,
  offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of
  Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl,
  inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09
  = MA DMF; 99 = Offshore deepwater (outside the stratified area). A
  change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has
  been in effect since Spring 1987, and may be summarized as follows:
  Previous strata: 01350; Present strata: 01351, 01352.

- TOW:

  Sequential number representing order in which station was selected
  within a stratum.

- SVVESSEL:

  Standard two character code for a survey vessel. Refer to
  SVDBS.SV_VESSEL

- YEAR:

  Year in which cruise was conducted.

- SEASON:

  Season of the year in which cruise was conducted.

- LAT:

  Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)

- LON:

  Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)

- EST_TOWDATE:

  Date and time represented by Eastern Standard Time (EST) for the start
  of a tow or deployment.(BEGIN_EST_TOWDATE)

- DEPTH:

  A four digit number recording the average depth, to the nearest meter,
  during a survey gear deployment.(AVGDEPTH)

- SURFTEMP:

  Surface temperature of water (degrees Celcius).

- BOTTEMP:

  Bottom temperature (degrees Celsius).

- SURFSALINITY:

  Salinity at water surface in practical salinity units (PSU).

- BOTSALINITY:

  Bottom salinity in Practical Salinity Units (PSU).

- SVSPP:

  A standard code which represents a species caught in a trawl or
  dredge. Refer to the SVDBS.SVSPECIES_LIST

- CATCHSEX:

  Code used to identify species that are sexed at the catch level. See
  SVDBS.SEX_CODES

- ABUNDANCE:

  Expanded number of individuals of a species caught at a given
  station.(EXPCATCHNUM)

- BIOMASS:

  Expanded catch weight of a species caught at a given station.
  (EXPCATCHWT)

The date:

- pullDate:

  The date the data was pulled from the database

The expression:

- functionCall:

  The call used to create the data pull

The version:

- version:

  The version of survdat used to pull the data

## See also

Other survdat:
[`calc_stratified_mean()`](https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.md),
[`calc_swept_area()`](https://noaa-edab.github.io/survdat/reference/calc_swept_area.md),
[`get_area()`](https://noaa-edab.github.io/survdat/reference/get_area.md),
[`get_survdat_clam_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_clam_data.md),
[`get_survdat_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_data.md),
[`get_survdat_scallop_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_scallop_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
channel <- dbutils::connect_to_database("serverName","userName")
get_mass_inshore_survey_data(channel)
} # }
```
