# Survey Design

``` r

library(survdat)
```

The Northeast Fisheries Science Center (NEFSC) has been conducting
standardized bottom trawl surveys in the fall since 1963 and spring
since 1968. The surveys follow a stratified random design. Fish species
and several invertebrate species are enumerated on a tow by tow basis
([Azarovitz 1981](#ref-Azarovitz_1981)). The data are housed in the
NEFSC’s survey database (SVDBS) maintained by the Ecosystem Survey
Branch.

Direct pulls from the database are not advisable as there have been
several gear modifications and vessel changes over the course of the
time series ([Miller et al. 2010](#ref-Miller_2010)). T R package
`survdat` was developed as a database query that applies the appropriate
calibration factors for a seamless time series since the 1960s.

The R package `survdat` is used to pull and process the data. `survdat`
identifies those research cruises associated with the seasonal bottom
trawl surveys and pulls the station and biological data. Station data
includes tow identification (cruise, station, and stratum), tow location
and date, as well as several environmental variables (depth,
surface/bottom salinity, and surface/bottom temperature). Stations are
filtered using a station, haul, gear (SHG) code for tows prior to 2009
and a tow, operations, gear, and aquisition (TOGA) code from 2009
onward. The codes that correspond to a representative tow (SHG \<= 136
or TOGA \<= 1324) are the same used by assessment biologists at the
NEFSC. Biological data includes the total biomass and abundance by
species, as well as lengths and number at length.

`survdat` applies the calibration factors. There are four calibrartion
factors applied (Table 64.1). Calibration factors are pulled directly
from SVDBS. Vessel conversions were made from either the NOAA Ship
**Delaware II** or NOAA Ship **Henry Bigelow** to the NOAA **Ship
Albatross IV** which was the primary vessel for most of the time series.
The Albatross was decommissioned in 2009 and the Bigelow is now the
primary vessel for the bottom trawl survey.

| Name                 | Code | Applied               |
|:---------------------|:-----|:----------------------|
| Door Conversion      | DCF  | \<1985                |
| Net Conversion       | GCF  | 1973 - 1981 (Spring)  |
| Vessel Conversion I  | VCF  | Delaware II records   |
| Vessel Conversion II | BCF  | Henry Bigelow records |

Calibration factors for NEFSC trawl survey data {.table}

Describe a little about the survey process that pertains to the data.

eg.

Since 19xx all species landed on the survey vessel are measured for
length. A subset of these are then individually weighed, sexed, assessed
for stage of maturity, and undergo a stomach content analysis

At the tow level … expanded number, expanded weight etc..

## References

Azarovitz, T. R. 1981. “A Brief Historical Review of the Woods Hole
Laboratory Trawl Survey Time Series.” *Canadian Special Publication of
Fisheries and Aquatic Sciences* 58: 62–67.

Miller, T. J., C. Das, P. J. Politis, et al. 2010. *Estimation of
Albatross IV to Henry B. Bigelow Calibration Factors*. Nos. 10-05.
National Marine Fisheries Service.
<https://www.nefsc.noaa.gov/publications/crd/crd1005/crd1005.pdf>.
