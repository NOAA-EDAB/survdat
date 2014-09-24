#Survdat_scallop.RData
#This script will generate data from the NEFSC sea scallop surveys
#Version 1.1
#5/2014
#V1.1 - added scallop regions and meat weight conversions
#SML

#-------------------------------------------------------------------------------
#User parameters
out.dir    <- "L:\\EcoAP\\Data\\survey"
shg.check  <- 'y' # y = use only SHG <=136 otherwise n
scall.only <- 'y' # y = grab only Atl. sea scallop (401)

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

#Increase memory size (max is 4096 in 32-bit R)
memory.limit(4000)

#Connect to Oracle
channel <- odbcDriverConnect()

#Generate cruise list
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 60
               and year >= 1963
               order by year, cruise6"

cruise <- as.data.table(sqlQuery(channel, cruise.qry))
cruise <- na.omit(cruise)
setkey(cruise, CRUISE6, SVVESSEL)

#Use cruise codes to select other data
cruise6 <- sqltext(cruise$CRUISE6)

#Station data
if(shg.check == 'y'){
  station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                 avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
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
scalldat <- merge(cruise, station)

#Catch data
if(scall.only == 'y'){
  catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
               from UNION_FSCS_SVCAT
               where cruise6 in (", cruise6, ")
               and svspp = '401'
               order by cruise6, station, svspp", sep='')
  }

if(scall.only == 'n'){
  catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
               from UNION_FSCS_SVCAT
               where cruise6 in (", cruise6, ")
               order by cruise6, station, svspp", sep='')
  }

catch <- as.data.table(sqlQuery(channel, catch.qry))
catch[, STRATUM := as.numeric(as.character(STRATUM))]

#Fix 1981 cruise with duplicate catch/length records
setkey(catch, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
abundance <- catch[, sum(ABUNDANCE), by = key(catch)]
biomass   <- catch[, sum(BIOMASS),   by = key(catch)]
setnames(abundance, "V1", "ABUNDANCE")
setnames(biomass,   "V1", "BIOMASS")
catch <- merge(abundance, biomass)

#merge with scalldat
setkey(catch, CRUISE6, STATION, STRATUM)
setkey(scalldat, CRUISE6, STATION, STRATUM)
scalldat <- merge(scalldat, catch, all.x = T)

#Length data
length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
              from UNION_FSCS_SVLEN
              where cruise6 in (", cruise6, ")
              order by cruise6, station, svspp, length", sep='')

len <- as.data.table(sqlQuery(channel, length.qry))
len[, STRATUM := as.numeric(as.character(STRATUM))]
setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX, LENGTH)

#fix 1981 cruise with duplicate catch/length records
len <- len[, sum(NUMLEN), by = key(len)]
setnames(len, "V1", "NUMLEN")

#merge with scalldat
setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
setkey(scalldat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
scalldat <- merge(scalldat, len, all.x = T)

odbcClose(channel)

#Assign Scallop Regions
regions <- c('MAB', 'GB')
MAB <- c(6010:6480, 6800:6960)
GB  <- 6490:6740

scalldat[, scall.region := factor(NA, levels = regions)]
for(i in 1:length(regions)) scalldat[STRATUM %in% get(regions[i]), scall.region := regions[i]]

#shell length-to-meat weight conversion coefficients (NEFSC 2001)
coeff <- data.table(scall.region = c('MAB',    'GB'),
                    a            = c(-12.2484, -11.6038),
                    b            = c( 3.2641,   3.1221))
coeff[, scall.region := as.factor(scall.region)]
scalldat <- merge(scalldat, coeff, by = 'scall.region')

#Lengths need to be in mm for formula to give g.  Divide by 1000 to get results in kg
scalldat[, meatwt := (exp(a) * (LENGTH * 10) ^ b) / 1000]
scalldat[, expmw := meatwt * NUMLEN]
scalldat[, stamw := sum(expmw), by = c('CRUISE6', 'STRATUM', 'STATION', 'SVSPP')]
scalldat[, c('a', 'b', 'meatwt', 'expmw') := NULL]
setnames(scalldat, "stamw", "BIOMASS.MW") 

save(scalldat, file = paste(out.dir, "\\Scalldat.RData", sep=''))





