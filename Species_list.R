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

#Grab cfspp by itis code
cfspp <- as.data.table(sqlQuery(channel, "select NESPP4, SPECIES_ITIS, 
                                COMMON_NAME, SCIENTIFIC_NAME 
                                from CFDBS.Species_itis_ne"))
setnames(cfspp, 'SPECIES_ITIS', 'ITISSPP')
cfspp[, NESPP3 := as.numeric(substr(sprintf('%04d', NESPP4), 1, 3))]
setkey(cfspp, NESPP3)
cfspp <- unique(cfspp)
cfspp[, NESPP4 := NULL]

#Merge to master species list
spp <- merge(svspp, cfspp, by = 'ITISSPP', all = T)
spp <- spp[!(is.na(SVSPP) & is.na(NESPP3)), ]

#Fix known issues
spp <- spp[!SVSPP %in% c(193, 310), ]

spp[ITISSPP == 630979, SVSPP  := 193] #Ocean Pout
spp[ITISSPP == 620992, SVSPP  := 310] #Deepsea red crab
spp[ITISSPP == 159772, NESPP3 := 150] #Hagfish
spp[ITISSPP == 166284, NESPP3 := 188] #John Dory
spp[ITISSPP == 98670,  NESPP3 := 714] #Cancer Crabs unk

spp[ITISSPP %in% c(630979, 620992), COMNAME := COMMON_NAME]
spp[ITISSPP %in% c(630979, 620992), SCINAME := SCIENTIFIC_NAME]

#Drop extra columns
spp[is.na(COMNAME), COMNAME := COMMON_NAME]
spp[is.na(SCINAME), SCINAME := SCIENTIFIC_NAME]
spp[, c('COMMON_NAME', 'SCIENTIFIC_NAME') := NULL]
setcolorder(spp, c('ITISSPP', 'SVSPP', 'NESPP3', 'COMNAME', 'SCINAME'))
setkey(spp, SVSPP, NESPP3)

#-------------------------------------------------------------------------------
#Add functional groups
#From NEFMC EBFM PDT work
#See Functional_group_table_Mike.csv in slucey/EcoAP/EBFM_PDT
spp[, EBFM.PDT := factor(NA, levels = c('Apex Predator', 'Benthivore', 'Benthos',
                                        'Macroplanktivore', 'Macrozoo-piscivore',
                                        'Mesoplanktivore', 'Piscivore', 'Other'))]
spp[SVSPP %in% c(11, 700, 704, 747), EBFM.PDT := 'Apex Predator']
spp[SVSPP %in% c(14, 22, 25, 74, 102, 105, 106, 107, 141, 143, 151, 164, 172,
                   176, 177, 192, 193, 301, 310, 312, 314, 317, 322, 501),
    EBFM.PDT := 'Benthivore']
spp[SVSPP %in% c(331, 336, 401, 403, 409), EBFM.PDT := 'Benthos']
spp[NESPP3 %in% c(775, 781, 805, 806), EBFM.PDT := 'Benthos']  
spp[SVSPP %in% c(76, 163, 168, 171, 502, 503), EBFM.PDT := 'Macroplanktivore']
spp[SVSPP %in% c(13, 24, 26, 27, 75, 77, 84, 108, 112, 155, 156, 311),
    EBFM.PDT := 'Macrozoo-piscivore']
spp[SVSPP %in% c(32, 33, 34, 35, 36, 121, 131), EBFM.PDT := 'Mesoplanktivore']
spp[SVSPP %in% c(15, 23, 28, 69, 72, 73, 101, 103, 104, 135, 139, 145, 197),
    EBFM.PDT := 'Piscivore']
spp[is.na(EBFM.PDT), EBFM.PDT := 'Other']


#-------------------------------------------------------------------------------                   
#Add EMAX q's
load(file.path(data.dir, 'EMAX_groups.RData'))
emax <- emax[!is.na(SVSPP), ]
spp <- merge(spp, emax[, list(SVSPP, EMAX, Fall.q, Spring.q)], by = 'SVSPP', all.x = T)

#Need to change emax abbreviations to actual names and add pelagics


#-------------------------------------------------------------------------------
save(spp, file = file.path(out.dir, 'Species_codes.RData'))

#-------------------------------------------------------------------------------
#Add Rpath box code - Still working!!!!
#Individual Species Groups
spp[SVSPP == 13, RPATH := 'SmoothDogfish']
spp[SVSPP == 15, RPATH := 'SpinyDogfish']
spp[SVSPP == 32, RPATH := 'AtlHerring']
spp[SVSPP == 35, RPATH := 'AmShad']
spp[SVSPP == 36, RPATH := 'AmMenhaden']
spp[SVSPP == 69, RPATH := 'OffHake']
spp[SVSPP == 72, RPATH := 'SilHake']
spp[SVSPP == 73, RPATH := 'Cod']
spp[SVSPP == 74, RPATH := 'Haddock']
spp[SVSPP == 75, RPATH := 'Pollock']
spp[SVSPP == 76, RPATH := 'WhHake']
spp[SVSPP == 77, RPATH := 'RedHake']
spp[SVSPP == 101, RPATH := 'AtlHalibut']
spp[SVSPP == 102, RPATH := 'AmPlaice']
spp[SVSPP == 103, RPATH := 'SummerFlounder']
spp[SVSPP == 104, RPATH := 'Fourspot']
spp[SVSPP == 105, RPATH := 'YTFlounder']
spp[SVSPP == 106, RPATH := 'WinterFlounder']
spp[SVSPP == 107, RPATH := 'WitchFlounder']
spp[SVSPP == 108, RPATH := 'Windowpane']
spp[SVSPP == 121, RPATH := 'AtlMackerel']
spp[SVSPP == 131, RPATH := 'Butterfish']
spp[SVSPP == 135, RPATH := 'Bluefish']
spp[SVSPP == 136, RPATH := 'AtlCroaker']
spp[SVSPP == 139, RPATH := 'StripedBass']
spp[SVSPP == 141, RPATH := 'BlackSeaBass']
spp[SVSPP == 143, RPATH := 'Scup']
spp[SVSPP == 145, RPATH := 'Weakfish']
spp[SVSPP == 151, RPATH := 'Tilefish']
spp[SVSPP == 155, RPATH := 'Redfish']
spp[SVSPP == 193, RPATH := 'OceanPout']
spp[SVSPP == 197, RPATH := 'Goosefish']
spp[SVSPP == 301, RPATH := 'AmLobster']
spp[SVSPP == 306, RPATH := 'Shrimp']
spp[SVSPP == 310, RPATH := 'RedCrab']
spp[SVSPP == 380, RPATH := 'AtlSturgeon']
spp[SVSPP == 401, RPATH := 'AtlScallop']
spp[SVSPP == 502, RPATH := 'Illex']
spp[SVSPP == 503, RPATH := 'Loligo']
spp[SVSPP == 654, RPATH := 'RedDrum']

#Multispecies Groups
spp[SVSPP %in% c(22:23), RPATH := 'LgSkates']
spp[SVSPP %in% c(24:28), RPATH := 'SmSkates']
spp[SVSPP %in% c(33, 34), RPATH := 'RiverHerring']
spp[SVSPP %in% c(30, 31, 37, 38, 43:46, 113, 124, 181, 208, 423, 426, 430, 475, 
                 476, 701, 702, 734, 744, 745, 851, 859, 865, 889),
    RPATH := 'SmPelagics']
spp[SVSPP %in% c(109, 117, 118), RPATH := 'SmFlatfishes']
spp[SVSPP %in% c(142, 149, 640, 645), RPATH := 'OtherScianids']
spp[SVSPP %in% c(403, 409), RPATH := 'Clams']

#Assign by common name
spp[COMNAME %like% 'SHARK' & !SVSPP %in% c(338, 349, 564), RPATH := 'Sharks']
spp[COMNAME %like% 'SHRIMP' & !SVSPP %in% c(306, 395, 787), RPATH := 'OtherShrimps']
spp[COMNAME %like% 'CRAB' & !(COMNAME %like% 'HERMIT' | COMNAME %like% 'RED')
    | SVSPP == 323, RPATH := 'Megabenthos']
spp[COMNAME %like% 'LOBSTER' & SVSPP != 301, RPATH := 'Megabenthos']
spp[COMNAME %like% 'STINGRAY' | 
      (COMNAME %like% 'RAY' & !(COMNAME %like% 'GRAY' | COMNAME %like% 'MORAY')),
    RPATH := 'Rays']
spp[COMNAME %like% 'TURTLE', RPATH := 'Turtles']
spp[COMNAME %like% 'TUNA', RPATH := 'HMS']
spp[COMNAME %like% 'MARLIN' & !SVSPP %in% c(91, 566), RPATH := 'HMS']
