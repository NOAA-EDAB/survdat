#' Extracts Clam data from Survey Database
#'
#'Connects to svdbs and pulls Clam & Quahog data from MSTR_CRUISE, UNION_FSCS_SVCAT, UNION_FSCS_SVLEN, UNION_FSCS_SVSTA.
#'Pulls from Cruises with purpose code = 50. (See \code{\url{get_cruise_purpose}}). Data are assigned to one of 7 regions
#'('SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB') and length-to-meat weight conversions applied
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to communicate with the database engine. (see \code{\link[dbutils]{connect_to_database}})
#' @param shg.check Boolean. use only SHG <=136 or TOGA <= 1324 (>2008). (Default = T)
#' @param clam.only Boolean. T = grab only Atl. surfclam (403) and ocean quahog (409)
#' @param tidy Boolean. Return output in long format (Default = F)
#'
#' @return A list containing a Data frame (data.table) (n x 21) and a list of SQL queries used to pull the data, the date of the pull, and the call expression
#' Each row of the data.table represents the number at length of a species on a specific tow along with physical attributes of the tow.
#'
#' The data frame (Descriptions taken from NEFSC Data dictionary)
#'
#' \item{clam.region}{One of 7 identified Regions, 'SVA', 'DMV', 'SNJ', 'NNJ', 'LI', 'SNE', 'GB'}
#' \item{CRUISE6}{Code uniquely identifying cruise. The first four digits indicate the year and the last two digit uniquely identify the cruise within the year. The 5th byte signifies cruises other than groundfish: Shrimp survey = 7 (i.e. 201470), State of Massachusetts survey = 9 (i.e. 201491), Food habits = 5 (i.e.199554)}
#' \item{STATION}{Unique sequential order in which stations have been completed. Hangups and short tows each receive a non-repeated consecutive number.}
#' \item{STRATUM}{	A predefined area where a net dredge, or other piece of gear was deployed. Code consists of 2 parts: Stratum group code number (2 bytes) and stratum number (3 bytes). Stratum group refers to if area fished is inshore or offshore North or South of Cape Hatteras or the type of cruise (shellfish, State of MA, offshore deepwater). The stratum number (third and fourth digits of code) refers to area defined by depth zone. See SVDBS.SVMSTRATA. The fifth digit of the code increases the length of the stratum number for revised strata after the Hague Line was established. Stratum group code: 01 = Trawl, offshore north of Hatteras; 02 = BIOM; 03 = Trawl, inshore north of Hatteras; 04 = Shrimp; 05 = Scotian shelf; 06 = Shellfish; 07 = Trawl, inshore south of Hatteras; 08 = Trawl, Offshore south of Hatteras; 09 = MA DMF; 99 = Offshore deepwater (outside the stratified area). A change in Bottom Trawl Stratum for the Gulf of Maine-Bay of Fundy has been in effect since Spring 1987, and may be summarized as follows: Previous strata: 01350; Present strata: 01351, 01352.}
#' \item{SVSPP}{A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#' \item{CATCHSEX}{Code used to identify species that are sexed at the catch level. See SVDBS.SEX_CODES}
#' \item{SVVESSEL}{Standard two character code for a survey vessel. Refer to SVDBS.SV_VESSEL}
#' \item{YEAR}{	Year in which cruise was conducted.}
#' \item{LAT}{Beginning latitude of tow in decimal degrees.(DECDEG_BEGLAT)}
#' \item{LON}{Beginning longitude of tow in decimal degrees.(DECDEG_BEGLON)}
#' \item{DEPTH}{	A four digit number recording the average depth, to the nearest meter, during a survey gear deployment.(AVGDEPTH)}
#' \item{SURFTEMP}{Surface temperature of water (degrees Celcius).}
#' \item{SURFSALIN}{Salinity at water surface in practical salinity units (PSU).}
#' \item{BOTTEMP}{Bottom temperature (degrees Celsius).}
#' \item{BOTSALIN}{Bottom salinity in Practical Salinity Units (PSU).}
#' \item{ABUNDANCE}{Expanded number of individuals of a species caught at a given station.(EXPCATCHNUM)}
#' \item{BIOMASS}{Expanded catch weight of a species caught at a given station. (EXPCATCHWT)}
#' \item{LENGTH}{Measured length of species in centimeters (cm). Measure method differs by species.}
#' \item{NUMLEN}{Expanded number of specimens at a given length.(EXPNUMLEN)}
#' \item{BIOMASS.MW}{Meat weight of catch based on LENGTH and NUMLEN of clams, conversion factors hard coded}
#'
#'
#' The list of sql statements:
#'
#' \item{cruise}{Select unique list of cruises. Table = MSTR_CRUISE}
#' \item{station}{Select unique set of stations from result of \code{cruise}. Table = UNION_FSCS_SVSTA}
#' \item{catch}{Select species abundance and biomass data from result of \code{station}. Table = UNION_FSCS_SVCAT}
#' \item{length}{Select Lengths of species found in \code{catch}. Table = UNION_FSCS_SVLEN}
#'
#' The date:
#'
#'  \item{pullDate}{The date the data was pulled from the database}
#'
#' The expression:
#'
#' \item{functionCall}{The call used to create the data pul}
#'
#'
#'@family survdat
#'
#'@examples
#'\dontrun{
#'# Recommended use:
#'channel <- dbutils::connect_to_database("serverName","userName")
#'get_survdat_clam_data(channel)
#'
#'}
#'
#'@export

#Nov. 16, 2023 Dan Hennen sent code to map old strata to new strata:
#The new strata are now divided into the 2 assessment regions for the purposes of meat weight and other biological parameter calculations. 
#The areas are south (strata 1-6) and north (strata 7-12). 
#This code maps old strata to new ones:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getRegNew=function(strata) ifelse(strata%in%c("1S","2S","3S","4S","5S","6S","1Q","2Q","3Q","4Q","5Q","6Q"),"South","GBK")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Convert from old strata to new strata - requires some additional arguments - see below:
StratumConvert=function(dt1,svspp=403,Conv=T){
  #dt1 is a data.frame that has at least the columns "Strata2" (Strata with the 6 and 0 removed), "Lat", "Lon", "Depth", "CRUISE6"
  #svspp is the species code for surfclams (403) or quahogs (409)
  dt1 <- dt1 |> dplyr::filter(!is.na(Depth))
  stratum2=dt1$Strata2 #Need to make sure we don't change the strata %in% 2018;
  stratum=paste(stratum2)
  lat=dt1$Lat
  lon=dt1$Lon
  #na.replace not working, package not available for R version 4.3.2
  #  depth=na.replace(dt1$Depth)
  depth=dt1$Depth
  cruise6=dt1$CRUISE6
  
  # CONVERT LATITUDE & LONGITUDE TO DECIMAL DEGREES;
  #-- latitude first;
  if(Conv){
    degrees=floor(lat/100);
    minutes=(lat-degrees*100)/60;
    lat=degrees+minutes;
    #-- longitude too;
    degrees=floor(lon/100);
    minutes=(lon-degrees*100)/60;
    lon=degrees+minutes;
    lon=-1*lon
  }
  #The formula for determ%in%%in%g if a po%in%t is one side or the other of a line is: ;
  # d=(lon-x[1])*(y[2]-y[1])-(lat-y[1])*(x[2]-x[1]) ;
  for(i in 1:nrow(dt1)) {
    if(stratum[i] %in% c('47') ) { #Need to determine the split of stratum[i] 47;
      if( ( (lon[i]-69.23)*(41-40)-(lat[i]-40)*(69.03-69.23) ) > 0 ) { stratum[i] = '471';
      } else {  stratum[i] = '472'}
    }
    
    if(stratum[i] %in% c('73') ) { #Need to determine the split of stratum[i] 73;
      if( ( (lon[i]-66.8)*(41.9-41.35)-(lat[i]-41.35)*(67.5-66.8) ) > 0) { stratum[i] = '73';
      } else {  stratum[i] = '74'}
    }
    
    #The Hudson canyon quahog split is complicated!;
    #This is a line along the bottom of Hudson Canyon;
    if(svspp %in% c(409) & stratum[i] %in% c('25', '26') & lat[i] >= 39.3 & lat[i]<= 40.2 ) { #Need to determine the split of strata 26 27 28;
      if( ( (lon[i]-72)*(40.2-39.3)-(lat[i]-39.3)*(73.75-72) ) < 0) { #above the line;
        if (stratum[i] %in% c('26')) { stratum[i] = '30'
        } else { if(stratum[i] %in% c('25')) { stratum[i] = '29'} }
      }
    }
    #This is the same line along the bottom of Hudson Canyon;
    if(svspp %in% c(409) & stratum[i] %in% c( '31', '32') ) { #Need to determine the split of strata 31 32;
      if( ( (lon[i]-72)*(40.2-39.3)-(lat[i]-39.3)*(73.75-72) ) < 0) { #below the line;
        if(stratum[i] %in% c('31')) { stratum[i] = '27'
        } else { if(stratum[i] %in% c('32')) { stratum[i] = '28'} }
      }
    }
    # This is a t%in%y line segment that is completely conta%in%ed %in% stratum[i] 25 & 26;
    if(svspp %in% c(409) & stratum[i] %in% c('25', '26') & lat[i] >= 40.2 & lat[i]<= 40.25) { #Need to determine the split of stratum[i] 26;
      if( ( (lon[i]-73.75)*(40.25-40.2)-(lat[i]-40.25)*(73.775-73.75) ) < 0) { #right of the line;
        if(stratum[i] %in% c('25')) { stratum[i] = '29'
        } else { if(stratum[i] %in% c('26')) { stratum[i] = '30'} }
      }
    }
    # This is a short line segment that is completely conta%in%ed %in% stratum[i] 25 & 26;
    if(svspp %in% c(409) & stratum[i] %in% c('25', '26') & lat[i] >= 40.25 & lat[i]<= 40.5) { #Need to determine the split of stratum[i] 26;
      if( ( (lon[i]-73.775)*(40.5-40.25)-(lat[i]-40.25)*(73.825-73.775) ) < 0) { #right of the line;
        if(stratum[i] %in% c('25')) { stratum[i] = '29'
        } else { if(stratum[i] %in% c('26')) { stratum[i] = '30'} }
      }
    }
    
    #Finally we have to split the southernmost big strata for oq only;
    if(svspp %in% c(409) & stratum[i] %in% c('17') ) { #Need to determine the split of strata 17;
      if( ( (lon[i]-74.29)*(38.6-38.94)-(lat[i]-38.94)*(74.57-74.29) ) < 0) { stratum[i] = '0'} #left of the line;
    }
    if(svspp %in% c(409) & stratum[i] %in% c('13') & lat[i] >= 38.41) { #Need to determine the split of stratum[i] 13;
      if( ( (lon[i]-74.57)*(38.41-38.6)-(lat[i]-38.6)*(74.64-74.57) ) < 0) { stratum[i] = '0'} #left of the line;
    }
    if(svspp %in% c(409) & stratum[i] %in% c('13') & lat[i] >= 38.15 & lat[i]<= 38.41) { #Need to determine the split of stratum[i] 13;
      if( ( (lon[i]-74.64)*(38.15-38.41)-(lat[i]-38.41)*(74.67-74.64) ) < 0) { stratum[i] = '0'} #left of the line;
    }
    if(svspp %in% c(409) & stratum[i] %in% c('13') & lat[i]<= 38.15) { #Need to determine the split of stratum[i] 13;
      if( ( (lon[i]-74.67)*(37.83-38.15)-(lat[i]-38.15)*(74.87-74.67) ) < 0) { stratum[i] = '0'} #left of the line;
    }
    
    #Now we can assign the new strata by the old strata;
    if (svspp %in% c(403) & (as.numeric(cruise6[i])/100)<2018) { #Surfclam
      if (stratum[i] %in% c('05','09','81') ) { stratum2[i] = "1S"}
      if (stratum[i] %in% c('84', '85', '86', '87')) { stratum2[i] = "2S"}  #NJ DMV %in%shore
      if (stratum[i] %in% c('13', '17', '21', '25', '29')) { stratum2[i] = "3S"}   #DMV, NJ & LI mid depth
      if (stratum[i] %in% c('10', '14', '18', '22')) { stratum2[i] = "4S"}   #NJ, DMV & LI off shore
      if (stratum[i] %in% c('88', '89', '90', '91', '92', '93')) { stratum2[i] = "5S"}   #LI & NJ %in%shore
      if (stratum[i] %in% c('45', '46', '95', '96')) { stratum2[i] = "6S"}  #SNE
      #GBK-surfs
      if (stratum[i] %in% c('53', '54')) { stratum2[i] = "7S" }  #NW deep
      if (stratum[i] %in% c('67', '69', '70')) { stratum2[i] = "8S" }  #West shallow
      if (stratum[i] %in% c('57', '58', '59', '60')) { stratum2[i] = "9S" }  #S deep
      if (stratum[i] %in% c('65', '66')) { stratum2[i] = "10S" }  #NE deep
      if (stratum[i] %in% c('68', '72', '73')) { stratum2[i] = "11S" }  #middle shallow
      if (stratum[i] %in% c('71', '74')) { stratum2[i] = "12S" }  #middle east shallow
      if (!stratum[i] %in% c('05','09','81','84', '85', '86', '87','13', '17', '21', '25', '29'
                             ,'10', '14', '18', '22','88', '89', '90', '91', '92', '93','45', '46', '95', '96'
                             ,'53', '54','67', '69', '70','57', '58', '59', '60','65', '66','68', '72', '73'
                             ,'71', '74')) {stratum2[i] = '0'}
    }
    
    if (svspp %in% c(409) & (as.numeric(cruise6[i])/100)<2018) { #Quahog
      if (stratum[i] %in% c('10', '11', '12', '14', '15', '16', '18', '19', '20')) { stratum2[i] = "1Q" }  #DMV deep
      if (stratum[i] %in% c('13', '17', '21', '22', '25', '26')) { stratum2[i] = "2Q"}   #NJ mid depth
      if (stratum[i] %in% c('23', '24', '27', '28', '31', '32', '35', '36')) { stratum2[i] = "3Q" }  #LI & NJ deep
      if (stratum[i] %in% c('29', '30', '33', '34')) { stratum2[i] = "4Q" }  #NJ & LI mid
      if (stratum[i] %in% c('92', '93', '94', '95', '37', '41')) { stratum2[i] = "5Q"  } #DMV/SVA
      if (stratum[i] %in% c('38', '39', '40', '46', '471', '48')) { stratum2[i] = "6Q" }  #NJ DMV %in%shore
      
      #GBK-Quahogs
      if (stratum[i] %in% c('53','54','55','56','472','56')) { stratum2[i] = "7Q"  } #West side deep
      if (stratum[i] %in% c('70')) { stratum2[i] = "8Q" }  #middle shallow
      if (stratum[i] %in% c('57','58','59','60')) { stratum2[i] = "9Q" }  #S deep
      if (stratum[i] %in% c('65','66')) { stratum2[i] = "10Q" }  #NE deep
      if (stratum[i] %in% c('74')) { stratum2[i] = "11Q" } #East side mid-depth
      if (stratum[i] %in% c('61','62')) { stratum2[i] = "12Q"  } #SE deep
      if(!stratum[i] %in% c('10', '11', '12', '14', '15', '16', '18', '19', '20','13', '17', '21', '22', '25', '26'
                            ,'23', '24', '27', '28', '31', '32', '35', '36', '29', '30', '33', '34','92', '93'
                            , '94', '95', '37', '41','38', '39', '40', '46', '471', '48', '53','54','55','56','472','56'
                            ,'70', '57','58','59','60','65','66','74','61','62') ) { stratum2[i] = '0'}
    }
    if(depth[i]>80) { stratum2[i] = '0'}
  }
  return(stratum2)
  
}
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#-------------------------------------------------------------------------------
#User parameters
get_survdat_clam_data <- function(channel,
                                  shg.check=T,
                                  clam.only=T,
                                  tidy = F) {

 # call <- capture_function_call()

  #Generate cruise list
  #V1.2 - remove surveys prior to 1982 due to difference in seasons/gear
  cruise.qry <- "select unique year, cruise6, svvessel
                 from mstr_cruise
                 where purpose_code = 50
                 and year >= 1982
                 order by year, cruise6"

  cruise <- data.table::as.data.table(DBI::dbGetQuery(channel, cruise.qry))
  cruise <- na.omit(cruise)
  data.table::setkey(cruise, CRUISE6, SVVESSEL)

  #Use cruise codes to select other data
  cruise6 <- survdat:::sqltext(cruise$CRUISE6)


  #Station data
  if(shg.check == T){
    station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from Union_fscs_svsta
                   where cruise6 in (", cruise6, ")
                   and SHG <= 136
                   order by cruise6, station", sep='')
    } else {
    station.qry <- paste("select unique cruise6, svvessel, station, stratum, decdeg_beglat as lat, decdeg_beglon as lon,
                   avgdepth as depth, surftemp, surfsalin, bottemp, botsalin
                   from UNION_FSCS_SVSTA
                   where cruise6 in (", cruise6, ")
                   order by cruise6, station", sep='')
    }

  station <- data.table::as.data.table(DBI::dbGetQuery(channel, station.qry))
  data.table::setkey(station, CRUISE6, SVVESSEL)

  #merge cruise and station
  clamdat <- base::merge(cruise, station)

  #Catch data
  if(clam.only == T){
    catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 and svspp in ('403', '409')
                 order by cruise6, station, svspp", sep='')
    } else {
    catch.qry <- paste("select cruise6, station, stratum, svspp, catchsex, expcatchnum as abundance, expcatchwt as biomass
                 from UNION_FSCS_SVCAT
                 where cruise6 in (", cruise6, ")
                 order by cruise6, station, svspp", sep='')
    }

  catch <- data.table::as.data.table(DBI::dbGetQuery(channel, catch.qry))
  data.table::setkey(catch, CRUISE6, STATION, STRATUM)

  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM)
  clamdat <- base::merge(clamdat, catch, all.x = T)

  #Length data
  if(clam.only == T){
    length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  and svspp in ('403', '409')
                  order by cruise6, station, svspp, length", sep='')
  } else {
    length.qry <- paste("select cruise6, station, stratum, svspp, catchsex, length, expnumlen as numlen
                  from UNION_FSCS_SVLEN
                  where cruise6 in (", cruise6, ")
                  order by cruise6, station, svspp, length", sep='')
  }

  len <- data.table::as.data.table(DBI::dbGetQuery(channel, length.qry))
  data.table::setkey(len, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

  #merge with clamdat
  data.table::setkey(clamdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
  clamdat <- base::merge(clamdat, len, all.x = T)

  clamdat[,STRATUM := as.numeric(STRATUM)]

  #Assign clam regions (old survey strata pre 2016)
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
  
  #subsets the stratum to stratum2 by deleting leading 06 
  clamdat[, stratum2 := substr(STRATUM, 2,3)]
  
  #rename variables to match Dan's strata change code:
 clamdat <-  clamdat %>% dplyr::rename(Lat = LAT, Lon = LON, Depth = DEPTH)

#Converting old strata to new using Dan Hennen's code:
  clamdat_test <- StratumConvert(clamdat) 
  
  return(clamdat)
  
  
  #shell length-to-meat weight conversion coefficients (OQ NEFSC 2004, SC NEFSC 2003)
  coeff <- data.table::data.table(clam.region = c('SVA',     'DMV',     'SNJ',     'NNJ',     'LI',      'SNE',     'GB'),
                      oq.a        = c(-9.04231,  -9.04231,  -9.84718,  -9.84718,  -9.23365,  -9.12428,  -8.96907),
                      oq.b        = c( 2.787987,  2.787987,  2.94954,   2.94954,   2.822474,  2.774989,  2.767282),
                      sc.a        = c(-7.0583,   -9.48913,  -9.3121,   -9.3121,   -7.9837,   -7.9837,   -8.27443),
                      sc.b        = c( 2.3033,    2.860176,  2.863716,  2.863716,  2.5802,    2.5802,    2.654215))
  coeff[, clam.region := as.factor(clam.region)]
  clamdat <- base::merge(clamdat, coeff, by = 'clam.region')

  #Lengths need to be in mm for formula to give g.  Divide by 1000 to get results in kg
  clamdat[SVSPP == 403, meatwt := (exp(sc.a) * (LENGTH * 10) ^ sc.b) / 1000]
  clamdat[SVSPP == 409, meatwt := (exp(oq.a) * (LENGTH * 10) ^ oq.b) / 1000]
  clamdat[, expmw := meatwt * NUMLEN]
  clamdat[, stamw := sum(expmw), by = c('CRUISE6', 'STRATUM', 'STATION', 'SVSPP')]
  clamdat[, c('oq.a', 'oq.b', 'sc.a', 'sc.b', 'meatwt', 'expmw') := NULL]
  data.table::setnames(clamdat, "stamw", "BIOMASS.MW")

  #saveRDS(clamdat, file = here::here('data","Clamdat.RDS'))
  if (tidy) {
    clamdat <- tibble::as_tibble(clamdat)
  }

  sql <- list(cruise=cruise.qry,station=station.qry,catch=catch.qry,length=length.qry)

  return(list(data=clamdat,sql=sql,pullDate=date(),functionCall = call))
}

