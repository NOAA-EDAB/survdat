# survdat

## Pull and process the NEFSC survey data

## Usage

To use this package you will need:

1.  To be behind the NEFSC firewall
2.  Permissions to access the required server : a username and password.
3.  Oracle’s instant Client installed
4.  Oracle’s ROracle R package

### Who this is for?

- Anyone looking for raw tow level survey data in the format that you
  would obtain from Ecosystem Survey Branch (ESB) - still in development

- Anyone looking for Ecosystem/multispecies products derived from the
  tow level data. See
  [vignettes](https://noaa-edab.github.io/survdat/articles/pullingData.md)
  for explanation of methods

### Who this isn’t for!

Anyone looking for survey data that is directly input into stock
assessments. You will need to either:

- Contact the appropriate stock assessment scientist
- Visit the [Northeast Region Stock Assessment Support
  Materials](https://www.fisheries.noaa.gov/resource/data/northeast-region-stock-assessment-support-materials)
- Visit [StockSMART web
  portal](https://apps-st.fisheries.noaa.gov/stocksmart?app=homepage) or
  [stocksmart](https://noaa-edab.github.io/stocksmart/) R package
  housing the data from the portal

## Installation

``` r
remotes::install_github("NOAA-EDAB/survdat")`
```

or

    pak::pak("NOAA-EDAB/survdat")

## Developers (in alphabetical order)

| [andybeet](https://github.com/andybeet) | [slucey](https://github.com/slucey) |
|----|----|
| [![](https://avatars1.githubusercontent.com/u/22455149?s=100&v=4)](https://github.com/andybeet "andybeet avatar") | [![](https://avatars.githubusercontent.com/u/5578254?s=100&u=cd59cd654cab73ea583c697145bfe062222355cd&v=4)](https://github.com/slucey "slucey avatar") |

#### Legal disclaimer

*This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.*

![NOAA
Fisheries](https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png)

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
