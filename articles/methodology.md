# How biomass estimates are calculated

`survdat` is a collection of tools that:

- Query the NEFSC survey database (SVDBS)
- Use the data to calculate estimates of biomass for species based on a
  geographic region

These data are available to qualified researchers upon request.

## Bottom Trawl Surveys

The Northeast Fisheries Science Center (NEFSC) has been conducting
standardized bottom trawl surveys in the fall since 1963 and spring
since 1968. The surveys follow a stratified random design. Fish species
and several invertebrate species are enumerated on a tow by tow basis
(Azarovitz 1981). The data are housed in the NEFSC’s survey database
(SVDBS) maintained by the Ecosystem Survey Branch.

Direct pulls from the database are not advisable as there have been
several gear modifications and vessel changes over the course of the
time series (Miller et al. 2010). `survdat` was developed as a database
query that applies the appropriate calibration factors for a seamless
time series since the 1960s.

## Methods

The `survdat` package can be broken down into two sections. The first
pulls the raw data from SVDBS. `survdat` identifies those research
cruises associated with the seasonal bottom trawl surveys and pulls the
station and biological data. Station data includes tow identification
(cruise, station, and stratum), tow location and date, as well as
several environmental variables (depth, surface/bottom salinity, and
surface/bottom temperature). Stations are filtered for representativness
using a station, haul, gear (SHG) code for tows prior to 2009 and a tow,
operations, gear, and aquisition (TOGA) code from 2009 onward. The codes
that correspond to a representative tow (SHG \<= 136 or TOGA \<= 1324)
are the same used by assessment biologists at the NEFSC. Biological data
includes the total biomass and abundance by species, as well as lengths
and number at length.

The second section of the Survdat script applies the calibration
factors. There are four calibrartion factors applied (Table
@ref(tab:calibration)). Calibration factors are pulled directly from
SVDBS. Vessel conversions were made from either the NOAA Ship *Delaware
II* or NOAA Ship *Henry Bigelow* to the NOAA Ship *Albatross IV* which
was the primary vessel for most of the time series. The Albatross was
decommisioned in 2009 and the Bigelow is now the primary vessel for the
bottom trawl survey.

| Name                 | Code | Applied               |
|:---------------------|:-----|:----------------------|
| Door Conversion      | DCF  | \<1985                |
| Net Conversion       | GCF  | 1973 - 1981 (Spring)  |
| Vessel Conversion I  | VCF  | Delaware II records   |
| Vessel Conversion II | BCF  | Henry Bigelow records |

Calibration factors for NEFSC trawl survey data {.table}

The output from `survdat` is a data.table that contains all the station
and biological data, corrected as noted above, from the NEFSC Spring
Bottom Trawl Survey and NEFSC Fall Bottom Trawl Survey.

For the purposes of the aggregate biomass indicators, fall and spring
survey data are treated separately. Additionally, all length data is
dropped and species seperated by sex at the catch level are merged back
together.

The data pulled from SVDBS is first post stratified into geographical
regions (any user supplied geospatial shapefile) by labeling sampling
stations by the geographical region they fell within (`post_strat.r`)
Next, the total number of stations within each geographical region per
year is counted using unique station records (`strat_prep.r`). Biomass
is summed by species per year per region and divided by the appropriate
station count to get the regional mean (`strat_mean.r`). Finally, the
mean biomasses are summed over the defined regions (`swept_area.r`).

## References

Azarovitz, T. R. 1981. “A Brief Historical Review of the Woods Hole
Laboratory Trawl Survey Time Series.” *Canadian Special Publication of
Fisheries and Aquatic Sciences* 58: 62–67.

Miller, T. J., C. Das, P. J. Politis, et al. 2010. *Estimation of
Albatross IV to Henry B. Bigelow Calibration Factors*. Nos. 10-05.
National Marine Fisheries Service.
<https://www.nefsc.noaa.gov/publications/crd/crd1005/crd1005.pdf>.
