% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_swept_area.R
\name{calc_swept_area}
\alias{calc_swept_area}
\title{Calculate swept area biomass}
\usage{
calc_swept_area(
  surveyData,
  areaPolygon = "NEFSC strata",
  areaDescription = "STRATA",
  filterByArea = "all",
  filterBySeason,
  groupDescription = "SVSPP",
  filterByGroup = "all",
  mergesexFlag = T,
  tidy = F,
  q = NULL,
  a = 0.0384
)
}
\arguments{
\item{surveyData}{Data table. NEFSC survey data generated by \code{get_survdat_data.R}}

\item{areaPolygon}{sf object or character string. Default = "NEFSC strata". The default option uses the survey strata shapefile bundled with the package.
To use any other shapefile for stratification, the shapefile must be read in as an \link[sf]{sf} object and the \code{areaDescription} argument must be specified.}

\item{areaDescription}{Character String. Column name from \code{areaPolygon}
that contains the strata designations.}

\item{filterByArea}{Numeric vector. Set of areas to subset from the
\code{areaDescription} of the \code{areaPolygon}.}

\item{filterBySeason}{Character string. Which seasons of the \code{surveyData}
should be included.  Choices include "SPRING", "FALL", or "all".}

\item{groupDescription}{Character string. Column of \code{prepData} which
contains the groups (e.g. "SVSPP") on which the means are based.}

\item{filterByGroup}{Character or numeric vector. Set of groups to subset from
\code{groupDescription}.  The default "all" will calculate means for all groups.}

\item{mergesexFlag}{Boolean. Logical value to merge sexed species such as dogfish.}

\item{tidy}{Boolean. Return output in long format (Default = F).}

\item{q}{Data frame. Table of survey catchabilities with a column corresponding
to \code{groupDescription} and a column of catchabilities.  If NULL, assumes
a \code{q} of 1 for each \code{groupDescription} (Minimum swept area estimates).}

\item{a}{Numeric. The average swept area of the trawl.  Default value is the
swept area of a standard NOAA Ship Albatross IV tow.}
}
\value{
data frame
}
\description{
Calculates the biomass. Details found here ...
}
\examples{
\dontrun{
# Pull data and apply conversion corrections
data <- get_survdat_data(channel)
# Calculate swept area biomass for specific survey strata for the SPRING season
calc_swept_area(surveyData=data$survdat, filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING")

# Calculate stratified mean for area defined by EPU regions, for all seasons ("SPRING", "FALL") and return in Tidy format
# Read in EPU shapefile (loaded as part of the package)
area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
calc_swept_area(surveyData=data$survdat, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all",tidy=T)

}


}
\seealso{
Other survdat: 
\code{\link{calc_stratified_mean}()},
\code{\link{get_area}()},
\code{\link{get_survdat_clam_data}()},
\code{\link{get_survdat_data}()},
\code{\link{get_survdat_scallop_data}()}
}
\concept{survdat}
