# Extracts Clam data from Survey Database

Connects to svdbs and pulls Clam & Quahog data from MSTR_CRUISE,
UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA. Pulls from Cruises
with purpose code = 50. (See
[`get_cruise_purpose`](https://noaa-edab.github.io/survdat/reference/get_cruise_purpose.md)).
Data are assigned to one of 2 new regions ('South', 'GBK') and
length-to-meat weight conversions applied.

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
  coefficients. (Default = T).

## Value

A list containing a Data frame (data.table) (n x 21) and a list of SQL
queries used to pull the data, the date of the pull, and the call
expression
