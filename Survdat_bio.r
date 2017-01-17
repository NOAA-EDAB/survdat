#Survdat_bio.RData
#This script will generate data from the NEFSC spring and fall bottom trawl surveys
#Version 1.0
#6/2013
#SML

#-------------------------------------------------------------------------------
#User parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey"
  out.dir  <- "L:\\EcoAP\\Data\\survey"
  memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
  data.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
  out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
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
if(Sys.info()['sysname']=="Windows"){
  channel <- odbcDriverConnect()
}else{
  channel <- odbcConnect('sole', uid, pwd)
}

#Read in Survdat
load(file.path(data.dir, 'Survdat.RData'))

#Use cruise codes to select other data
cruise6 <- sqltext(unique(survdat[, CRUISE6]))

#Bio data
bio.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, indid,
                indwt, sex, maturity, age, stom_volume
                from UNION_FSCS_SVBIO
                where cruise6 in (", cruise6, ")", sep = '')
                
bio <- as.data.table(sqlQuery(channel, bio.qry))

#Fix catch sex prior to 2001
bio[is.na(CATCHSEX), CATCHSEX := 0L]
bio[SVSPP %in% c(15, 301) & SEX == 1 & CRUISE6 < 200100, CATCHSEX := 1L]
bio[SVSPP %in% c(15, 301) & SEX == 2 & CRUISE6 < 200100, CATCHSEX := 2L]

setkey(bio, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX, LENGTH)

#Remove YT Stratum
bio <- bio[!STRATUM %like% 'YT', ]
bio[, STRATUM := as.numeric(as.character(STRATUM))]

#merge with survdat
setkey(survdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX, LENGTH)
survdat.bio <- merge(survdat, bio)

odbcClose(channel)

survdat.bio <- survdat.bio[!is.na(INDWT), ]
save(survdat.bio, file = file.path(out.dir, "SurvdatBio.RData"))





