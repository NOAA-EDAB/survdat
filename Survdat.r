#Survdat.r
#This script will generate data from the NEFSC bottom trawl surveys
#SML

#-------------------------------------------------------------------------------
#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\Rworkspace\\RSurvey"
  out.dir  <- "L:\\EcoAP\\Data\\survey"
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rworkspace/RSurvey"
  out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
}

uid      <- 'slucey'
cat("Oracle Password: ")
pwd <- scan(stdin(), character(), n = 1)

shg.check  <- 'y' # y = use only SHG <=136 or TOGA <= 1324 (>2008)
raw.check  <- 'n' # y = save data without conversions (survdat.raw), will still 
                  #     save data with conversions (survdat)
all.season <- 'n' # y = save data with purpose code 10 not just spring/fall 
                  #     (survdat.allseason), will not save survdat regular
use.SAD    <- 'n' # y = grab data from Survey Analysis Database (SAD) for 
                  #     assessed species
#-------------------------------------------------------------------------------
#Required packages
library(RODBC); library(data.table)

#-------------------------------------------------------------------------------
#Created functions
  #Convert output to text for RODBC query
sqltext <- function(x){
  out <- x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      out <- paste(out, x[i], sep = "','")
    }
  }
  out <- paste("'", out, "'", sep = '')
  return(out)
}

#-------------------------------------------------------------------------------
#Begin script
channel <- odbcConnect('sole', uid, pwd)

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
  preHB.station.qry <- paste("select unique cruise6, svvessel, station, stratum,
                             tow, decdeg_beglat as lat, decdeg_beglon as lon, 
                             begin_est_towdate as est_towdate, avgdepth as depth, 
                             surftemp, surfsalin, bottemp, botsalin
                             from Union_fscs_svsta
                             where cruise6 in (", cruise6, ")
                             and SHG <= 136
                             and cruise6 <= 200900
                             order by cruise6, station", sep='')
  
  HB.station.qry <- paste("select unique cruise6, svvessel, station, stratum,
                          tow, decdeg_beglat as lat, decdeg_beglon as lon, 
                          begin_est_towdate as est_towdate, avgdepth as depth, 
                          surftemp, surfsalin, bottemp, botsalin
                          from Union_fscs_svsta
                          where cruise6 in (", cruise6, ")
                          and TOGA <= 1324
                          and cruise6 > 200900
                          order by cruise6, station", sep='')
  
  preHB.sta <- as.data.table(sqlQuery(channel, preHB.station.qry))
  HB.sta    <- as.data.table(sqlQuery(channel, HB.station.qry))
  station   <- rbindlist(list(preHB.sta, HB.sta))
  }

if(shg.check == 'n'){
  station.qry <- paste("select unique cruise6, svvessel, station, stratum, tow,
                       decdeg_beglat as lat, decdeg_beglon as lon, 
                       begin_est_towdate as est_towdate, avgdepth as depth, 
                       surftemp, surfsalin, bottemp, botsalin
                       from UNION_FSCS_SVSTA
                       where cruise6 in (", cruise6, ")
                       order by cruise6, station", sep='')
  station <- as.data.table(sqlQuery(channel, station.qry))
  }
  
setkey(station, CRUISE6, SVVESSEL)

#merge cruise and station
survdat <- merge(cruise, station)


#Catch data
catch.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex, 
                   expcatchnum as abundance, expcatchwt as biomass
                   from UNION_FSCS_SVCAT
                   where cruise6 in (", cruise6, ")
                   and stratum not like 'YT%'
                   order by cruise6, station, svspp", sep='')

catch <- as.data.table(sqlQuery(channel, catch.qry))
setkey(catch, CRUISE6, STATION, STRATUM, TOW)

#merge with survdat
setkey(survdat, CRUISE6, STATION, STRATUM, TOW)
survdat <- merge(survdat, catch, by = key(survdat))

#Length data
length.qry <- paste("select cruise6, station, stratum, tow, svspp, catchsex, 
                    length, expnumlen as numlen
                    from UNION_FSCS_SVLEN
                    where cruise6 in (", cruise6, ")
                    and stratum not like 'YT%'
                    order by cruise6, station, svspp, length", sep='')

len <- as.data.table(sqlQuery(channel, length.qry))
setkey(len, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)

#merge with survdat
setkey(survdat, CRUISE6, STATION, STRATUM, TOW, SVSPP, CATCHSEX)
survdat <- merge(survdat, len, all.x = T)

if(raw.check == 'y'){
  survdat.raw <- survdat
  save(survdat.raw, file = paste(out.dir, "Survdat_raw.RData", sep =''))
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

#Bigelow >2008 Vessel Conversion 
#Use Bigelow conversions for Pisces as well (PC)
#Tables 53-55 from Miller et al. 2010 - number estimators
big.abund <- data.table(svspp = c(12, 22, 24, 27, 28, 31, 33, 34, 73, 76, 106, 107, 
                                  109, 121, 135, 136, 141, 143, 145, 149, 155, 164, 
                                  171, 181, 193, 197, 502, 512, 15, 23, 26, 32, 72,
                                  74, 77, 78, 102, 103, 104, 105, 108, 131, 163, 301,
                                  313, 401, 503, 15, 23, 26, 32, 72, 74, 77, 78, 102, 
                                  103, 104, 105, 108, 131, 163, 301, 313, 401, 503),
                        season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                        rho = c(1.161, 4.44, 6.689, 4.384, 3.792, 1.227, 1.323, 1.29,
                                1.987, 2.235, 2.49, 3.257, 8.249, 1.188, 1.16, 1.134,
                                3.416, 1.705, 1.577, 1.541, 1.456, 1.802, 4.494, 0.57,
                                4.575, 7.129,	1.38, 2.309, 1.202, 3.822, 3.08, 2.287,
                                6.283, 0.972, 3.959, 3.839, 2.074, 3.226, 3.099, 2.347,
                                3.311, 1.487, 3.56, 1.571, 3.343, 3.965, 2.034, 1.204,
                                2.609, 9.846, 2, 4.354, 0.243, 2.662, 2.319, 2.16, 
                                2.405, 2.356, 2.366, 0.2, 0.172, 3.114, 1.586, 2.511,
                                3.166, 0.84))

#Tables 56-58 from Miller et al. 2010 Biomass estimators
big.bio <- data.table(svspp = c(12, 22, 24, 27, 28, 31, 33, 34, 73, 76, 106, 107, 
                                109, 121, 135, 136, 141, 143, 145, 149, 155, 164, 
                                171, 181, 193, 197, 502, 512, 15, 23, 26, 32, 72,
                                74, 77, 78, 102, 103, 104, 105, 108, 131, 163, 301,
                                313, 401, 503, 15, 23, 26, 32, 72, 74, 77, 78, 102, 
                                103, 104, 105, 108, 131, 163, 301, 313, 401, 503),
                      season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                      rhoW = c(1.082, 3.661, 6.189, 4.45, 3.626, 1.403, 1.1, 2.12,
                               1.58, 2.088, 2.086, 3.257, 12.199, 0.868, 0.665, 1.125,
                               2.827, 1.347, 1.994, 1.535, 1.191, 1.354, 3.259, 0.22,
                               3.912, 8.062, 1.409, 2.075, 1.166, 3.718, 2.786, 5.394,
                               4.591, 0.878, 3.712, 3.483, 2.092, 3.066, 3.05, 2.244,
                               3.069, 2.356, 2.986, 1.272, 3.864, 1.85, 2.861, 1.21, 
                               2.174, 8.814, 1.95, 4.349, 1.489, 3, 2.405, 1.692,
                               2.141, 2.151, 2.402, 1.901, 1.808, 2.771, 1.375, 2.479,
                               3.151, 1.186))

#ID species to convert
spp.both <- big.abund[season == 'both', svspp]
spp.seasonal <- big.abund[season == 'fall', svspp]

#Both seasons  
for(ispp in 1:length(spp.both)){
  survdat[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp.both[ispp], 
          BIOMASS := BIOMASS / big.bio[svspp == spp.both[ispp], rhoW]]
  survdat[SVVESSEL %in% c('HB', 'PC') & SVSPP == spp.both[ispp],
          ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.both[ispp], rho])]
}
#Seperate calibration factors by season
for(ispp in 1:length(spp.seasonal)){
  survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'SPRING' & SVSPP == spp.seasonal[ispp],
          BIOMASS := BIOMASS / big.bio[svspp == spp.seasonal[ispp] & season == 'spring' , 
                                       rhoW]]
  
  survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'FALL' & SVSPP == spp.seasonal[ispp],
          BIOMASS := BIOMASS / big.bio[svspp == spp.seasonal[ispp] & season == 'fall' , 
                                       rhoW]]
  
  survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'SPRING' & SVSPP == spp.seasonal[ispp],
          ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.seasonal[ispp] & 
                                                     season == 'spring' , rho])]
  
  survdat[SVVESSEL %in% c('HB', 'PC') & SEASON == 'FALL' & SVSPP == spp.seasonal[ispp],
          ABUNDANCE := round(ABUNDANCE / big.abund[svspp == spp.seasonal[ispp] & 
                                                     season == 'fall' , rho])]
  }



#Need to update...no longer SAD now ADIOS.  Table structure has changed
if(use.SAD == 'y'){
  sad.qry <- "select svspp, cruise6, stratum, tow, station, sex as catchsex,
             catch_wt_B_cal, catch_no_B_cal, length, length_no_B_cal
             from STOCKEFF.I_SV_MERGED_CATCH_CALIB_O"
  sad     <- as.data.table(sqlQuery(channel, sad.qry))
  
  setkey(sad, CRUISE6, STRATUM, TOW, STATION, SVSPP, CATCHSEX, LENGTH)
  sad <- unique(sad)
  survdat <- merge(survdat, sad, by = key(sad), all.x = T)
  
  #Carry over SAD values to survdat columns and delete SAD columns
  survdat[!is.na(CATCH_WT_B_CAL),  BIOMASS   := CATCH_WT_B_CAL]
  survdat[!is.na(CATCH_NO_B_CAL),  ABUNDANCE := CATCH_NO_B_CAL]
  survdat[, NUMLEN := as.double(NUMLEN)]
  survdat[!is.na(LENGTH_NO_B_CAL), NUMLEN    := LENGTH_NO_B_CAL]
  survdat[, c('CATCH_WT_B_CAL', 'CATCH_NO_B_CAL', 'LENGTH_NO_B_CAL') := NULL]
}

odbcClose(channel)

if(all.season == 'n') save(survdat, file = file.path(out.dir, "Survdat.RData"))
if(all.season == 'y') save(survdat, file = file.path(out.dir, 
                                                     "Survdat_allseason.RData"))




