# Extracts Clam data from Survey Database

Connects to svdbs and pulls Clam & Quahog data from MSTR_CRUISE,
UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA. Pulls from Cruises
with purpose code = 50. (See
[`get_cruise_purpose`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose)).
Data are assigned to one of 7 regions ('SVA', 'DMV', 'SNJ', 'NNJ', 'LI',
'SNE', 'GB') and length-to-meat weight conversions applied

## Usage

``` r
get_survdat_clam_data(
  channel,
  shg.check = T,
  clam.only = T,
  tidy = F,
  assignRegionWeights = T
)
```

## Arguments

- channel:

  an Object inherited from
  [DBIConnection-class](https://dbi.r-dbi.org/reference/DBIConnection-class.html).
  This object is used to communicate with the database engine. (see
  `connect_to_database`)

- shg.check:

  Boolean. use only SHG \<=136 or TOGA \<= 1324 (\>2008). (Default = T)

- clam.only:

  Boolean. T = grab only Atl. surfclam (403) and ocean quahog (409)

- tidy:

  Boolean. Return output in long format (Default = F)

- assignRegionWeights:

  Boolean. Assign Strata to Regions and then apply length weight
  coefficients. Currently hard coded for Survey strata prior to 2017.
  (Default = T).

## Value

A list containing a Data frame (data.table) (n x 21) and a list of SQL
queries used to pull the data, the date of the pull, and the call
expression Each row of the data.table represents the number at length of
a species on a specific tow along with physical attributes of the tow.

The data frame (Descriptions taken from NEFSC Data dictionary)

- clam.region:

  One of 7 identified Regions, 'SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE',
  'GB'

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

- SVSPP:

  A standard code which represents a species caught in a trawl or
  dredge. Refer to the SVDBS.SVSPECIES_LIST

- CATCHSEX:

  Code used to identify species that are sexed at the catch level. See
  SVDBS.SEX_CODES

- SVVESSEL:

  Standard two character code for a survey vessel. Refer to
  SVDBS.SV_VESSEL

- YEAR:

  Year in which cruise was conducted.

- LAT:

  Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)

- LON:

  Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)

- DEPTH:

  A four digit number recording the average depth, to the nearest meter,
  during a survey gear deployment.(AVGDEPTH)

- SURFTEMP:

  Surface temperature of water (degrees Celcius).

- SURFSALIN:

  Salinity at water surface in practical salinity units (PSU).

- BOTTEMP:

  Bottom temperature (degrees Celsius).

- BOTSALIN:

  Bottom salinity in Practical Salinity Units (PSU).

- ABUNDANCE:

  Expanded number of individuals of a species caught at a given
  station.(EXPCATCHNUM)

- BIOMASS:

  Expanded catch weight of a species caught at a given station.
  (EXPCATCHWT)

- LENGTH:

  Measured length of species in centimeters (cm). Measure method differs
  by species.

- NUMLEN:

  Expanded number of specimens at a given length.(EXPNUMLEN)

- BIOMASS.MW:

  Meat weight of catch based on LENGTH and NUMLEN of clams, conversion
  factors hard coded

The list of sql statements:

- cruise:

  Select unique list of cruises. Table = MSTR_CRUISE

- station:

  Select unique set of stations from result of `cruise`. Table =
  UNION_FSCS_SVSTA

- catch:

  Select species abundance and biomass data from result of `station`.
  Table = UNION_FSCS_SVCAT

- length:

  Select Lengths of species found in `catch`. Table = UNION_FSCS_SVLEN

The date:

- pullDate:

  The date the data was pulled from the database

The expression:

- functionCall:

  The call used to create the data pul

## See also

Other survdat:
[`calc_stratified_mean()`](https://noaa-edab.github.io/survdat/reference/calc_stratified_mean.md),
[`calc_swept_area()`](https://noaa-edab.github.io/survdat/reference/calc_swept_area.md),
[`get_area()`](https://noaa-edab.github.io/survdat/reference/get_area.md),
[`get_mass_inshore_survey_data()`](https://noaa-edab.github.io/survdat/reference/get_mass_inshore_survey_data.md),
[`get_survdat_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_data.md),
[`get_survdat_scallop_data()`](https://noaa-edab.github.io/survdat/reference/get_survdat_scallop_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Recommended use:
channel <- dbutils::connect_to_database("serverName","userName")
get_survdat_clam_data(channel)

} # }
```
