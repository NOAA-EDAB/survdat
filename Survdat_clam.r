#Survdat.RData
#This script will generate data from the NEFSC surfclam and ocean quahog surveys
#Version 1.2
#5/2014
#V1.2 - remove surveys prior to 1982 due to difference in seasons/gear
#V1.1 - added clam regions and meat weight conversions
#SML

#-------------------------------------------------------------------------------
#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\Rworkspace\\RSurvey\\"
  out.dir  <- "L:\\EcoAP\\Data\\survey\\"
  memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rworkspace/RSurvey/"
  out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey/"
  uid      <- 'slucey'
  cat("Oracle Password: ")
  pwd <- scan(stdin(), character(), n = 1)
}

shg.check <- 'y' # y = use only SHG <=136 otherwise n
clam.only <- 'y' # y = grab only Atl. surfclam (403) and ocean quahog (409)

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
#Connect to Oracle
if(Sys.info()['sysname']=="Windows"){
  channel <- odbcDriverConnect()
} else {
  channel <- odbcConnect('sole', uid, pwd)
}

#Generate cruise list
cruise.qry <- "select unique year, cruise6, svvessel
               from mstr_cruise
               where purpose_code = 50
               and year >= 1982
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
clamdat <- merge(cruise, station)

#Catch data
if(clam.only == 'y'){
  catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
               from UNION_FSCS_SVCAT
               where cruise6 in (", cruise6, ")
               and svspp in ('403', '409')
               order by cruise6, station, svspp", sep='')
  }

if(clam.only == 'n'){
  catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
               from UNION_FSCS_SVCAT
               where cruise6 in (", cruise6, ")
               order by cruise6, station, svspp", sep='')
  }
  
catch <- as.data.table(sqlQuery(channel, catch.qry))
setkey(catch, CRUISE6, STATION, STRATUM)

#merge with clamdat
setkey(clamdat, CRUISE6, STATION, STRATUM)
clamdat <- merge(clamdat, catch, all.x = T)

#Length data
length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
              from UNION_FSCS_SVLEN
              where cruise6 in (", cruise6, ")
              order by cruise6, station, svspp, length", sep='')

len <- as.data.table(sqlQuery(channel, length.qry))
setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

#merge with clamdat
setkey(clamdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
clamdat <- merge(clamdat, len, all.x = T)

odbcClose(channel)

#Assign clam regions
regions <- c('SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB')
SVA <- c(6010:6080, 6800, 6810)
DMV <- c(6090:6160, 6820:6860)
SNJ <- c(6170:6200, 6870)
NNJ <- c(6210:6280, 6880:6900)
LI  <- c(6290:6360, 6910:6930)
SNE <- c(6370:6520, 6940:6960)
GB  <- c(6530:6740)

clamdat[, clam.region := factor(NA, levels = regions)]
for(i in 1:length(regions)) clamdat[STRATUM %in% get(regions[i]), clam.region := regions[i]]

#shell length-to-meat weight conversion coefficients (OQ NEFSC 2004, SC NEFSC 2003)
coeff <- data.table(clam.region = c('SVA',     'DMV',     'SNJ',     'NNJ',     'LI',      'SNE',     'GB'),
                    oq.a        = c(-9.04231,  -9.04231,  -9.84718,  -9.84718,  -9.23365,  -9.12428,  -8.96907),
                    oq.b        = c( 2.787987,  2.787987,  2.94954,   2.94954,   2.822474,  2.774989,  2.767282),
                    sc.a        = c(-7.0583,   -9.48913,  -9.3121,   -9.3121,   -7.9837,   -7.9837,   -8.27443),
                    sc.b        = c( 2.3033,    2.860176,  2.863716,  2.863716,  2.5802,    2.5802,    2.654215))
coeff[, clam.region := as.factor(clam.region)]
clamdat <- merge(clamdat, coeff, by = 'clam.region')

#Lengths need to be in mm for formula to give g.  Divide by 1000 to get results in kg
clamdat[SVSPP == 403, meatwt := (exp(sc.a) * (LENGTH * 10) ^ sc.b) / 1000]
clamdat[SVSPP == 409, meatwt := (exp(oq.a) * (LENGTH * 10) ^ oq.b) / 1000]
clamdat[, expmw := meatwt * NUMLEN]
clamdat[, stamw := sum(expmw), by = c('CRUISE6', 'STRATUM', 'STATION', 'SVSPP')]
clamdat[, c('oq.a', 'oq.b', 'sc.a', 'sc.b', 'meatwt', 'expmw') := NULL]
setnames(clamdat, "stamw", "BIOMASS.MW")   

save(clamdat, file = file.path(out.dir, "Clamdat.RData"))





