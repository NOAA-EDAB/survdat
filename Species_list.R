#Species list
if(Sys.info()['sysname']=="Windows"){
  out.dir  <- "L:\\EcoAP\\Data\\survey"
  memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
  out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
  uid      <- 'slucey'
  cat("Oracle Password: ")
  pwd <- scan(stdin(), character(), n = 1)
}

library(RODBC); library(data.table)

if(Sys.info()['sysname']=="Windows"){
  channel <- odbcDriverConnect()
} else {
  channel <- odbcConnect('sole', uid, pwd)
}

#Grab svspp code by itis code
svspp <- as.data.table(sqlQuery(channel, "select SVSPP, ITISSPP, COMNAME, SCINAME 
                                from ITIS_Lookup"))
setkey(svspp, SVSPP)
svspp <- unique(svspp)

#Grab cfspp by itis code
cfspp <- as.data.table(sqlQuery(channel, "select NESPP4, SPECIES_ITIS, 
                                COMMON_NAME, SCIENTIFIC_NAME 
                                from CFDBS.Species_itis_ne"))
setnames(cfspp, 'SPECIES_ITIS', 'ITISSPP')
cfspp[, NESPP3 := as.numeric(substr(sprintf('%04d', NESPP4), 1, 3))]
setkey(cfspp, NESPP3)
cfspp <- unique(cfspp)

#Merge to master species list
spp <- merge(svspp, cfspp, by = 'ITISSPP', all = T)

#Drop extra columns
spp[is.na(COMNAME), COMNAME := COMMON_NAME]
spp[is.na(SCINAME), SCINAME := SCIENTIFIC_NAME]
spp[, c('NESPP4', 'COMMON_NAME', 'SCIENTIFIC_NAME') := NULL]
setcolorder(spp, c('ITISSPP', 'SVSPP', 'NESPP3', 'COMNAME', 'SCINAME'))
setkey(spp, SVSPP, NESPP3)

save(spp, file = file.path(out.dir, 'Species_codes.RData'))
