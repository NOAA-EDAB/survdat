#Survdat.RData
#This script will generate data from the NEFSC spring and fall bottom trawl surveys
#Version 1.1
#Added feature to output all survey data regardless of season
#5/2014
#SML

#-------------------------------------------------------------------------------
#User parameters
if(Sys.info()['sysname']=="Windows"){
    data.dir <- "L:\\Rworkspace\\RSurvey"
    out.dir  <- "L:\\EcoAP\\Data\\survey"
    memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
    data.dir <- "/home/slucey/slucey/Rworkspace/RSurvey"
    out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
    uid      <- 'slucey'
    cat('Oracle Password:')
    pwd <- readLines(n=1) #If reading from source, need to manually add pwd here
}

shg.check  <- 'y' # y = use only SHG <=136 otherwise n
raw.check  <- 'n' # y = save data without conversions (survdat.raw), will still 
#     save data with conversions (survdat)
all.season <- 'y' # y = save data with purpose code 10 not just spring/fall 
#     (survdat.allseason), will not save survdat regular

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

#Connect to Oracle
if(Sys.info()['sysname']=="Windows"){
    channel <- odbcDriverConnect()
} else {
    channel <- odbcConnect('sole', uid, pwd)
}

#Generate cruise list
if(all.season == 'n'){
  cruise.qry <- "select unique year, cruise6, svvessel, season
    from mstr_cruise
    where purpose_code = 10
    and year >= 1963
    and (season = 'FALL'
      or season = 'SPRING')
    order by year, cruise6"
  }

if(all.season == 'y'){
  cruise.qry <- "select unique year, cruise6, svvessel, season
    from mstr_cruise
    where purpose_code = 10
    and year >= 1963
    order by year, cruise6"
  }
    
cruise <- as.data.table(sqlQuery(channel, cruise.qry))
cruise <- na.omit(cruise)
setkey(cruise, CRUISE6, SVVESSEL)

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Station data
if(shg.check == 'y'){
  station.qry <- paste("select unique id, status_code, est_year, est_month, est_day, est_julian_day, est_time,
                        cruise6, svvessel, station, stratum, statype, towdur, rpm, dopdistb, dopdistw, botspeed,
                        bottemp, surftemp, botsalin, surfsalin, mindepth, maxdepth, avgdepth, setdepth, enddepth,
                        decdeg_beglat, decdeg_endlat, decdeg_beglon, decdeg_endlon, area, airtemp, cloud, baropress,
                        winddir, windsp, wavehgt
                        from Union_fscs_svsta
                        where cruise6 in (", cruise6, ")
                        and SHG <= 136
                        order by cruise6, station", sep='')
  }

if(shg.check == 'n'){
  station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
    avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
    from UNION_FSCS_SVSTA
    where cruise6 in (", cruise6, ")
    order by cruise6, station", sep='')
  }
  
station <- as.data.table(sqlQuery(channel, station.qry))
setkey(station, CRUISE6, SVVESSEL)

#merge cruise and station
survdat <- merge(cruise, station, by = key(cruise))

#Catch data
catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, 
                   expcatchwt as biomass
                   from UNION_FSCS_SVCAT
                   where cruise6 in (", cruise6, ")
                   and stratum not like 'YT%'
                   order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))
setkey(catch, CRUISE6, STATION, STRATUM)

#merge with survdat
setkey(survdat, CRUISE6, STATION, STRATUM)
survdat <- merge(survdat, catch, by = key(survdat))

spp <- as.data.table(sqlQuery(channel, "select svspp, comname from SVSPECIES_LIST"))
setkey(spp, SVSPP)
spp <- unique(spp)

survdat <- merge(survdat, spp, by = 'SVSPP', all.x = T)

#Length data
length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                    from UNION_FSCS_SVLEN
                    where cruise6 in (", cruise6, ")
                    and stratum not like 'YT%'
                    order by cruise6, station, svspp, length", sep='')

len <- as.data.table(sqlQuery(channel, length.qry))
setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

#merge with survdat
setkey(survdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
survdat <- merge(survdat, len, by = key(survdat), all.x = T)

if(raw.check == 'y'){
  survdat.raw <- survdat
  save(survdat.raw, file = file.path(out.dir, "Survdat_raw.RData"))
  }

#Conversion Factors
#need to make abundance column a double instead of an integer
survdat[, ABUNDANCE := as.double(ABUNDANCE)]

#Grab all conversion factors off the network
convert.qry <- "select *
  from survan_conversion_factors"

convert <- as.data.table(sqlQuery(channel,convert.qry))

#DCF < 1985 Door Conversion
dcf.spp <- convert[DCF_WT > 0, SVSPP]
for(i in 1:length(dcf.spp)){
  survdat[YEAR < 1985 & SVSPP == dcf.spp[i],
      BIOMASS := BIOMASS * convert[SVSPP == dcf.spp[i], DCF_WT]]
  }
dcf.spp <- convert[DCF_NUM > 0, SVSPP]
for(i in 1:length(dcf.spp)){
  survdat[YEAR < 1985 & SVSPP == dcf.spp[i],
      ABUNDANCE := round(ABUNDANCE * convert[SVSPP == dcf.spp[i], DCF_NUM])]
  }

#GCF Spring 1973-1981  Net Conversion
gcf.spp <- convert[GCF_WT > 0, SVSPP]
for(i in 1:length(gcf.spp)){
  survdat[SEASON == 'SPRING' & YEAR > 1972 & YEAR < 1982 & SVSPP == gcf.spp[i],
      BIOMASS := BIOMASS / convert[SVSPP == gcf.spp[i], GCF_WT]]
  }
gcf.spp <- convert[GCF_NUM > 0, SVSPP]
for(i in 1:length(gcf.spp)){
  survdat[SEASON == 'SPRING' & YEAR > 1972 & YEAR < 1982 & SVSPP == gcf.spp[i],
      ABUNDANCE := round(ABUNDANCE / convert[SVSPP == gcf.spp[i], GCF_NUM])]
  }

#VCF SVVESSEL = DE  Vessel Conversion
vcf.spp <- convert[VCF_WT > 0, SVSPP]
for(i in 1:length(vcf.spp)){
  survdat[SVVESSEL == 'DE' & SVSPP == vcf.spp[i],
      BIOMASS := BIOMASS * convert[SVSPP == vcf.spp[i], VCF_WT]]
  }
vcf.spp <- convert[VCF_NUM > 0, SVSPP]
for(i in 1:length(vcf.spp)){
  survdat[SVVESSEL == 'DE' & SVSPP == vcf.spp[i],
      ABUNDANCE := round(ABUNDANCE * convert[SVSPP == vcf.spp[i], VCF_NUM])]
  }

#Bigelow >2008 Vessel Conversion - need flat files (not on network)
big.fall   <- as.data.table(read.csv(file.path(data.dir, 'bigelow_fall_calibration.csv')))
big.spring <- as.data.table(read.csv(file.path(data.dir, 'bigelow_spring_calibration.csv')))

bf.spp <- big.fall[pW != 1, svspp]
for(i in 1:length(bf.spp)){
  survdat[SVVESSEL == 'HB' & SEASON == 'FALL' & SVSPP == bf.spp[i],
      BIOMASS := BIOMASS / big.fall[svspp == bf.spp[i], pW]]
  }
bf.spp <- big.fall[pw != 1, svspp]
for(i in 1:length(bf.spp)){
  survdat[SVVESSEL == 'HB' & SEASON == 'FALL' & SVSPP == bf.spp[i],
      ABUNDANCE := round(ABUNDANCE / big.fall[svspp == bf.spp[i], pw])]
  }

bs.spp <- big.spring[pW != 1, svspp]
for(i in 1:length(bs.spp)){
  survdat[SVVESSEL == 'HB' & SEASON == 'SPRING' & SVSPP == bs.spp[i],
      BIOMASS := BIOMASS / big.spring[svspp == bs.spp[i], pW]]
  }
bs.spp <- big.spring[pw != 1, svspp]
for(i in 1:length(bs.spp)){
  survdat[SVVESSEL == 'HB' & SEASON == 'SPRING' & SVSPP == bs.spp[i],
      ABUNDANCE := round(ABUNDANCE / big.spring[svspp == bs.spp[i], pw])]
  }

odbcClose(channel)

survdat[, YEAR := NULL]
setcolorder(survdat, c('ID', 'STATUS_CODE', 'EST_YEAR', 'EST_MONTH', 'EST_DAY', 'EST_JULIAN_DAY', 'EST_TIME',
                       'SEASON', 'SVVESSEL', 'STATYPE', 'TOWDUR', 'RPM', 'DOPDISTB', 'DOPDISTW', 'BOTSPEED',
                       'BOTTEMP', 'SURFTEMP', 'BOTSALIN', 'SURFSALIN', 'MINDEPTH', 'MAXDEPTH', 'AVGDEPTH', 'SETDEPTH',
                       'ENDDEPTH', 'STRATUM', 'DECDEG_BEGLAT', 'DECDEG_ENDLAT', 'DECDEG_BEGLON', 'DECDEG_ENDLON',
                       'SVSPP', 'CATCHSEX', 'COMNAME', 'BIOMASS', 'ABUNDANCE', 'AREA', 'AIRTEMP', 'CLOUD', 'BAROPRESS',
                       'WINDDIR', 'WINDSP', 'WAVEHGT', 'CRUISE6', 'STATION', 'LENGTH', 'NUMLEN'))


if(all.season == 'n') save(survdat, file = file.path(out.dir, "Survdat_Nye.RData"))
if(all.season == 'y') save(survdat, file = file.path(out.dir, "Survdat_Nye_allseason.RData"))




