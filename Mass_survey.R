#Data pull of Mass inshore survey

#-------------------------------------------------------------------------------
#User parameters
windows <- F
if(windows == T){
  data.dir <- "L:\\EcoAP\\Data\\survey\\"
  out.dir  <- "L:\\EcoAP\\Data\\survey\\"
  memory.limit(4000)
}
if(windows == F){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/Data/survey/"
  uid      <- 'slucey'
  cat('Oracle Password:')
  pwd <- readLines(n=1) #If reading from source, need to manually add pwd here
}

#-------------------------------------------------------------------------------
#Required packages
library(RODBC); library(data.table)

#-------------------------------------------------------------------------------
#Created functions
#Convert output to text for RODBC query
sqltext<-function(x){
  out<-x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      out<-paste(out, x[i], sep="','")
    }
  }
  out<-paste("'", out, "'", sep='')
  return(out)
}

#-------------------------------------------------------------------------------
#Begin script
if(windows == T){
  channel <- odbcDriverConnect()
}else{
  channel <- odbcConnect('sole', uid, pwd)
}

#Select cruises
cruise.qry <- "select unique year, cruise6, svvessel, season
    from mstr_cruise
    where purpose_code = 11
    and year >= 1963
    order by year, cruise6"

cruise <- as.data.table(sqlQuery(channel, cruise.qry))

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Select station data
station.qry <- paste("select unique cruise6, station, stratum, shg, decdeg_beglat as lat, 
                       decdeg_beglon as lon, begin_est_towdate as est_towdate, avgdepth as depth, 
                       surftemp, surfsalin, bottemp, botsalin
                       from UNION_FSCS_SVSTA
                       where cruise6 in (", cruise6, ")
                       order by cruise6, station", sep='')


station <- as.data.table(sqlQuery(channel, station.qry))

#merge cruise and station
mass.survey <- merge(cruise, station, by = 'CRUISE6')

#Grab catch data
catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, 
                   expcatchwt as biomass
                   from UNION_FSCS_SVCAT
                   where cruise6 in (", cruise6, ")
                   and stratum not like 'YT%'
                   order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))
setkey(catch, CRUISE6, STATION, STRATUM)

#merge with mass.survey
setkey(mass.survey, CRUISE6, STATION, STRATUM)
mass.survey <- merge(mass.survey, catch, by = key(mass.survey))

#Length data
length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                    from UNION_FSCS_SVLEN
                    where cruise6 in (", cruise6, ")
                    and stratum not like 'YT%'
                    order by cruise6, station, svspp, length", sep='')

len <- as.data.table(sqlQuery(channel, length.qry))
setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

#merge with mass.survey
setkey(mass.survey, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
mass.survey <- merge(mass.survey, len, all.x = T, allow.cartesian = T)

odbcClose(channel)

save(mass.survey, file = paste(out.dir, "Mass_survey.RData", sep=''))




