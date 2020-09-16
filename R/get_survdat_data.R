#' Extracts Survey data  from Database
#'
#'Connects to svdbs and pulls data from MSTR_CRUISE, UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param all.season Boolean. Spring and Fall only (F) otherwise T. Default = F
#' @param shg.check Boolean. use only SHG <=136 or TOGA <= 1324 (>2008) (T). Default = T
#'
#' @return A list containing a Data frame (data.table) (n x 21) and a list of SQL queries used to pull the data
#' Each row of the data.table represents the number at length of a species on a specific tow along with physical  attributes of the tow.
#'
#' The data frame
#'
#' \item{CRUISE6}{}
#' \item{STATION}{}
#' \item{STRATUM}{}
#' \item{TOW}{}
#' \item{SVSPP}{}
#' \item{CATCHSEX}{}
#' \item{SVVESSEL}{}
#' \item{YEAR}{}
#' \item{SEASON}{}
#' \item{LAT}{}
#' \item{LON}{}
#' \item{EST_TOWDATE}{}
#' \item{DEPTH}{}
#' \item{SURFTEMP}{}
#' \item{SURFSALIN}{}
#' \item{BOTTEMP}{}
#' \item{BOTSALIN}{}
#' \item{ABUNDANCE}{}
#' \item{BIOMASS}{}
#' \item{LENGTH}{}
#' \item{NUMLEN}{}
#'
#' The list of sql statements:
#'
#' \item{cruise}{Select unique list of cruises. Table = MSTR_CRUISE}
#' \item{station}{Select unique set of stations from result of \code{cruise}. Table = UNION_FSCS_SVSTA}
#' \item{catch}{Select species abundance and biomass data from result of \code{station}. Table = UNION_FSCS_SVCAT}
#' \item{length}{Select Lengths of species found in \code{catch}. Table = UNION_FSCS_SVLEN}
#'
#'
#'
#'@export

get_survdat_data <- function(channel,all.season=F,shg.check=T,raw.check=F) {

  #shg.check  <- 'y' # y = use only SHG <=136 or TOGA <= 1324 (>2008)
  raw.check  <- 'n' # y = save data without conversions (survdat.raw), will still
                    #     save data with conversions (survdat)
  #all.season <- 'n' # y = save data with purpose code 10 not just spring/fall
                    #     (survdat.allseason), will not save survdat regular
  use.SAD    <- 'n' # y = grab data from Survey Analysis Database (SAD) for
                    #     assessed species


  # Cruise List --------------------------------------------------------------
  #Generate cruise list
  message("Getting Cruise list  ...")
  if(all.season == F){ # Spring and Fall
    cruise.qry <- "select unique year, cruise6, svvessel, season
      from mstr_cruise
      where purpose_code = 10
      and year >= 1963
      and (season = 'FALL'
        or season = 'SPRING')
      order by year, cruise6"
   } else if(all.season == T){ # Everything
    cruise.qry <- "select unique year, cruise6, svvessel, season
      from mstr_cruise
      where purpose_code = 10
      and year >= 1963
      order by year, cruise6"
   }


  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  setkey(cruise, CRUISE6, SVVESSEL)

  #Use cruise codes to select other data
  cruise6 <- sqltext(cruise$CRUISE6)



  # Station Data --------------------------------------------------------------
  message("Getting Station data ...")
  if(shg.check == T){
    station.qry <- paste("select unique cruise6, svvessel, station, stratum,
                               tow, decdeg_beglat as lat, decdeg_beglon as lon,
                               begin_est_towdate as est_towdate, avgdepth as depth,
                               surftemp, surfsalin, bottemp, botsalin
                               from UNION_FSCS_SVSTA
                               where cruise6 in (", cruise6, ")
                               and (SHG <= 136 and cruise6 <= 200900)
                               or (TOGA <= 1324 and cruise6 > 200900)
                               order by cruise6, station", sep='')

    # pull data
    station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))

  } else if(shg.check == F){
    station.qry <- paste("select unique cruise6, svvessel, station, stratum, tow,
                         decdeg_beglat as lat, decdeg_beglon as lon,
                         begin_est_towdate as est_towdate, avgdepth as depth,
                         surftemp, surfsalin, bottemp, botsalin
                         from UNION_FSCS_SVSTA
                         where cruise6 in (", cruise6, ")
                         order by cruise6, station", sep='')
    # pull data
    station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  }

  data.table::setkey(station, CRUISE6, SVVESSEL)
  #merge cruise and station
  survdat <- merge(cruise, station)


  # Catch Data --------------------------------------------------------------
  message("Getting Species data ...")
  catch.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex,
                     expcatchnum as abundance, expcatchwt as biomass
                     from UNION_FSCS_SVCAT
                     where cruise6 in (", cruise6, ")
                     and stratum not like 'YT%'
                     order by cruise6, station, svspp", sep='')
  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  data.table::setkey(catch, CRUISE6, STATION, STRATUM, TOW)

  #merge with survdat
  data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW)
  survdat <- merge(survdat, catch, by = key(survdat))



  # Length Data --------------------------------------------------------------
  message("Getting Length Data ...")
  #Length data
  length.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex,
                      length, expnumlen as numlen
                      from UNION_FSCS_SVLEN
                      where cruise6 in (", cruise6, ")
                      and stratum not like 'YT%'
                      order by cruise6, station, svspp, length", sep='')
  len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
  data.table::setkey(len, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)

  #merge with survdat
  data.table::setkey(survdat, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)
  survdat <- merge(survdat, len, all.x = T)

  #if(raw.check == 'y'){
    #survdat.raw <- survdat
#    save(survdat.raw, file = paste(out.dir, "Survdat_raw.RData", sep =''))
  #}

  sql <- list(catch=catch.qry, cruise=cruise.qry, length=length.qry, station=station.qry)

  return(list(survdat=survdat,sql=sql))

}
