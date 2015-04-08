#Survdat_wgtATlen.r
#Add weight at length and size categories to survdat.RData
#7/14
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey\\"
  out.dir  <- "L:\\EcoAP\\Data\\survey\\"
  r.dir    <- "L:\\Rworkspace\\RSurvey\\"
  gis.dir  <- "L:\\Rworkspace\\GIS_files"
}

if(Sys.info()['sysname']=="Linux"){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/Data/survey/"
  r.dir    <- "slucey/Rworkspace/RSurvey/"
  gis.dir  <- "slucey/Rworkspace/GIS_files"
  uid <- 'slucey'
  cat('Oracle Password:')
  pwd <- readLines(n=1)
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(RODBC)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
load(paste(data.dir, "survdat.RData", sep = ''))

#Need a length/weight coefficients
if(Sys.info()['sysname']=="Windows") channel <- odbcDriverConnect()
if(Sys.info()['sysname']=="Linux")   channel <- odbcConnect('sole', uid, pwd)
  
lw.qry <- "select svspp, sex, svlwexp,        svlwcoeff,
                              svlwexp_spring, svlwcoeff_spring,
                              svlwexp_fall,   svlwcoeff_fall,
                              svlwexp_winter, svlwcoeff_winter,
                              svlwexp_summer, svlwcoeff_summer
           from length_weight_coefficients"
           
lw <- as.data.table(sqlQuery(channel, lw.qry))

#fix NAs
lw[is.na(SVLWEXP_SPRING), SVLWEXP_SPRING := SVLWEXP]
lw[is.na(SVLWEXP_FALL),   SVLWEXP_FALL   := SVLWEXP]
lw[is.na(SVLWEXP_WINTER), SVLWEXP_WINTER := SVLWEXP]
lw[is.na(SVLWEXP_SUMMER), SVLWEXP_SUMMER := SVLWEXP]

lw[is.na(SVLWCOEFF_SPRING), SVLWCOEFF_SPRING := SVLWCOEFF]
lw[is.na(SVLWCOEFF_FALL),   SVLWCOEFF_FALL   := SVLWCOEFF]
lw[is.na(SVLWCOEFF_WINTER), SVLWCOEFF_WINTER := SVLWCOEFF]
lw[is.na(SVLWCOEFF_SUMMER), SVLWCOEFF_SUMMER := SVLWCOEFF]

#Merge with survdat
setnames(lw, "SEX", "CATCHSEX")
survdat.lw <- merge(survdat, lw, by = c('SVSPP', 'CATCHSEX'), all.x = T)

#Calculate weight at length
survdat.lw[SEASON == 'SPRING', INDWT := exp(SVLWCOEFF_SPRING + SVLWEXP_SPRING * log(LENGTH))]
survdat.lw[SEASON == 'FALL',   INDWT := exp(SVLWCOEFF_FALL   + SVLWEXP_FALL   * log(LENGTH))]
survdat.lw[SEASON == 'WINTER', INDWT := exp(SVLWCOEFF_WINTER + SVLWEXP_WINTER * log(LENGTH))]
survdat.lw[SEASON == 'SUMMER', INDWT := exp(SVLWCOEFF_SUMMER + SVLWEXP_SUMMER * log(LENGTH))]

#Calculate expanded weight at length
survdat.lw[, WGTLEN := INDWT * NUMLEN]

#Remove extra columns
survdat.lw[, c('SVLWEXP',        'SVLWCOEFF',
               'SVLWEXP_SPRING', 'SVLWCOEFF_SPRING',
               'SVLWEXP_FALL',   'SVLWCOEFF_FALL',
               'SVLWEXP_WINTER', 'SVLWCOEFF_WINTER',
               'SVLWEXP_SUMMER', 'SVLWCOEFF_SUMMER') := NULL]
               
#Assign size categories from FH
survdat.lw[, SIZECAT := factor(NA, levels = c('XS', 'S', 'M', 'L', 'XL'))]
survdat.lw[LENGTH <= 20,                SIZECAT := 'S']
survdat.lw[LENGTH >  20 & LENGTH <= 50, SIZECAT := 'M']
survdat.lw[LENGTH >  50,                SIZECAT := 'L']

#then set for specific taxa
#dogfish
#/*if (svspp>=013 and svspp<=015) and pdlen<=40 then sizecat='S ';
#else if (svspp>=013 and svspp<=015) and pdlen>40 and pdlen<=60 then sizecat='M ';
#else if (svspp>=013 and svspp<=015) and pdlen>60 and pdlen<=80 then sizecat='L ';*/

#new dogfish
survdat.lw[SVSPP %in% 13:15 & LENGTH <  36,               SIZECAT := 'S']
survdat.lw[SVSPP %in% 13:15 & LENGTH >= 36 & LENGTH < 80, SIZECAT := 'M']
survdat.lw[SVSPP %in% 13:15 & LENGTH >= 80,               SIZECAT := 'L']
#skates
survdat.lw[SVSPP %in% 20:29 & LENGTH <= 30,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 20:29 & LENGTH >  30 & LENGTH <= 60, SIZECAT := 'M']
survdat.lw[SVSPP %in% 20:29 & LENGTH >  60 & LENGTH <= 80, SIZECAT := 'L']
survdat.lw[SVSPP %in% 20:29 & LENGTH >  80,                SIZECAT := 'XL']
#anchovies, smelt, greeneyes, lanternfishes
survdat.lw[SVSPP %in% 38:56 & LENGTH <= 10,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 38:56 & LENGTH >  10 & LENGTH <= 25, SIZECAT := 'M']
survdat.lw[SVSPP %in% 38:56 & LENGTH >  25,                SIZECAT := 'L']
#sandlance, hookear sculpin, pearlsides
survdat.lw[SVSPP %in% c(181, 159, 229) & LENGTH <= 10,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(181, 159, 229) & LENGTH >  10 & LENGTH <= 25, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(181, 159, 229) & LENGTH >  25,                SIZECAT := 'L']
#clupeids
survdat.lw[SVSPP %in% 30:37 & LENGTH <= 10,                SIZECAT := 'XS']
survdat.lw[SVSPP %in% 30:37 & LENGTH >  10 & LENGTH <= 20, SIZECAT := 'S']
survdat.lw[SVSPP %in% 30:37 & LENGTH >  20 & LENGTH <= 30, SIZECAT := 'M']
survdat.lw[SVSPP %in% 30:37 & LENGTH >  30,                SIZECAT := 'L']
#Atlantic mackerel, butterfish
survdat.lw[SVSPP %in% c(121, 131) & LENGTH <= 10,                SIZECAT := 'XS']
survdat.lw[SVSPP %in% c(121, 131) & LENGTH >  10 & LENGTH <= 20, SIZECAT := 'S']
survdat.lw[SVSPP == 121 & LENGTH > 20 & LENGTH <= 35,            SIZECAT := 'M']
survdat.lw[SVSPP == 121 & LENGTH > 35,                           SIZECAT := 'L']
survdat.lw[SVSPP == 131 & LENGTH > 20 & LENGTH <= 30,            SIZECAT := 'M']
survdat.lw[SVSPP == 131 & LENGTH > 30,                           SIZECAT := 'L']
#hakes
survdat.lw[SVSPP %in% 76:79 & LENGTH <= 20,                  SIZECAT := 'S']
survdat.lw[SVSPP %in% 76:79 & LENGTH >  20 & LENGTH <= 40,   SIZECAT := 'M']
survdat.lw[SVSPP %in% 76:79 & LENGTH >  40,                  SIZECAT := 'L']

survdat.lw[SVSPP %in% c(69, 72, 81, 86, 87) & LENGTH <= 20,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(69, 72, 81, 86, 87) & LENGTH >  20 & LENGTH <= 40, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(69, 72, 81, 86, 87) & LENGTH >  40,                SIZECAT := 'L']
#cod, haddock, pollock
survdat.lw[SVSPP %in% 73:75 & LENGTH <= 20,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 73:75 & LENGTH >  20 & LENGTH <= 50, SIZECAT := 'M']
survdat.lw[SVSPP %in% 73:75 & LENGTH >  50 & LENGTH <= 80, SIZECAT := 'L']
survdat.lw[SVSPP %in% 73:75 & LENGTH >  80,                SIZECAT := 'XL']
#flatfish
survdat.lw[SVSPP %in% 102:110 & LENGTH <= 20,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 102:110 & LENGTH >  20 & LENGTH <= 40, SIZECAT := 'M']
survdat.lw[SVSPP %in% 102:110 & LENGTH >  40 & LENGTH <= 70, SIZECAT := 'L']
survdat.lw[SVSPP %in% 102:110 & LENGTH >  70,                SIZECAT := 'XL']
#halibut
survdat.lw[SVSPP == 101 & LENGTH <= 30,                SIZECAT := 'S']
survdat.lw[SVSPP == 101 & LENGTH >  30 & LENGTH <= 60, SIZECAT := 'M']
survdat.lw[SVSPP == 101 & LENGTH >  60 & LENGTH <= 90, SIZECAT := 'L']
survdat.lw[SVSPP == 101 & LENGTH >  90,                SIZECAT := 'XL']
#bluefish, striped bass
survdat.lw[SVSPP %in% c(135, 139) & LENGTH <= 30,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(135, 139) & LENGTH >  30 & LENGTH <= 70, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(135, 139) & LENGTH >  70,                SIZECAT := 'L']
#croaker, black sea bass, weakfish
survdat.lw[SVSPP %in% c(136, 141, 145) & LENGTH <= 25,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(136, 141, 145) & LENGTH >  25 & LENGTH <= 50, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(136, 141, 145) & LENGTH >  50,                SIZECAT := 'L']
#longhorn sculpin, sea raven, redfish
survdat.lw[SVSPP %in% c(155, 163, 164) & LENGTH <= 25,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(155, 163, 164) & LENGTH >  25 & LENGTH <= 50, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(155, 163, 164) & LENGTH >  50,                SIZECAT := 'L']
#sea robins
survdat.lw[SVSPP %in% 171:174 & LENGTH <= 20,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 171:174 & LENGTH >  20 & LENGTH <= 30, SIZECAT := 'M']
survdat.lw[SVSPP %in% 171:174 & LENGTH >  30,                SIZECAT := 'L']
#goosefish
survdat.lw[SVSPP == 197 & LENGTH <= 30,                SIZECAT := 'S']
survdat.lw[SVSPP == 197 & LENGTH >  30 & LENGTH <= 60, SIZECAT := 'M']
survdat.lw[SVSPP == 197 & LENGTH >  60 & LENGTH <= 90, SIZECAT := 'L']
survdat.lw[SVSPP == 197 & LENGTH >  90,                SIZECAT := 'XL']
#pouts, wolffish
survdat.lw[SVSPP %in% 189:193 & LENGTH <= 30,                SIZECAT := 'S']
survdat.lw[SVSPP %in% 189:193 & LENGTH >  30 & LENGTH <= 60, SIZECAT := 'M']
survdat.lw[SVSPP %in% 189:193 & LENGTH >  60,                SIZECAT := 'L']
#squids
survdat.lw[SVSPP %in% c(502, 503) & LENGTH <= 15,                SIZECAT := 'S']
survdat.lw[SVSPP %in% c(502, 503) & LENGTH >  15 & LENGTH <= 30, SIZECAT := 'M']
survdat.lw[SVSPP %in% c(502, 503) & LENGTH >  30,                SIZECAT := 'L']

#Sum biomass by size category
setkey(survdat.lw, CRUISE6, STRATUM, STATION, SVSPP, CATCHSEX, SIZECAT)

survdat.lw[, WGTCAT := sum(WGTLEN), by = key(survdat.lw)]
survdat.lw[, NUMCAT := sum(NUMLEN), by = key(survdat.lw)]

survdat.lw <- unique(survdat.lw)

#Drop individual columns
survdat.lw[, c('LENGTH', 'NUMLEN', 'INDWT', 'WGTLEN') := NULL]

save(survdat.lw, file = paste(out.dir, "survdat_lw.RData", sep = ''))
