#Poststrat.r   v2.1
#post stratify survdat.r based on GIS shapefile
#6/13
#SML


poststrat <- function (x, stratum = 'EPU', strata.col = 'EPU', na.keep = F) {
  #x is the data to stratify
  #stratum    <- r object containing the shapefile
  #strata.col <- stratum@data column that contains strata designations
  #na.keep    <- keep old strata names in output

  #-------------------------------------------------------------------------------
  #Required packages
  library(rgdal); library(data.table)
  
  #-------------------------------------------------------------------------------
  #User created functions
  
  #-------------------------------------------------------------------------------
 
  #Data
  x <- as.data.table(x)
          
  #Convert to spatial points data frame
  coordinates(x) <- ~LON+LAT
  x@proj4string <- CRS('+init=epsg:4326') #Lat/Lon code
  lcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ") #Lambert Conformal Conic
  x<-spTransform(x, lcc)
  
  #Identify tows within new strata
  stratum <- spTransform(get(stratum), lcc)
  x$newstrata <- over(x, stratum)[ ,strata.col]
  
  #Output data (convert spatial data frame back to lat/lon)
  x <- spTransform(x, CRS('+init=epsg:4326'))
  x.data <- as.data.table(as.data.frame(x))
  if(na.keep == F) out <- x.data[!is.na(newstrata), ]
  if(na.keep == T) out <- x.data

  return(out)
  }