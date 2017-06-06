#Species list
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "L:\\EcoAP\\Data\\survey"
  out.dir  <- "L:\\EcoAP\\Data\\survey"
  memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/EcoAP/Data/survey"
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

setkey(cfspp, ITISSPP, NESPP3)
cfspp <- unique(cfspp)

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

spp <- spp[!SCIENTIFIC_NAME %like% 'HIPPOGLOSUS', ]

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
#Add Rpath box code
#Individual Species Groups
spp[SVSPP == 13,  RPATH := 'SmoothDogfish']
spp[SVSPP == 15,  RPATH := 'SpinyDogfish']
spp[SVSPP == 22,  RPATH := 'Barndoor']
spp[SVSPP == 23,  RPATH := 'WinterSkate']
spp[SVSPP == 26,  RPATH := 'LittleSkate']
spp[SVSPP == 32,  RPATH := 'AtlHerring']
spp[SVSPP == 35,  RPATH := 'AmShad']
spp[SVSPP == 36,  RPATH := 'AmMenhaden']
spp[SVSPP == 69,  RPATH := 'OffHake']
spp[SVSPP == 72,  RPATH := 'SilverHake']
spp[SVSPP == 73,  RPATH := 'Cod']
spp[SVSPP == 74,  RPATH := 'Haddock']
spp[SVSPP == 75,  RPATH := 'Pollock']
spp[SVSPP == 76,  RPATH := 'WhiteHake']
spp[SVSPP == 77,  RPATH := 'RedHake']
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
spp[SVSPP == 155, RPATH := 'Redfish']
spp[SVSPP == 193, RPATH := 'OceanPout']
spp[SVSPP == 197, RPATH := 'Goosefish']
spp[SVSPP == 301, RPATH := 'AmLobster']
spp[SVSPP == 306, RPATH := 'NShrimp']
spp[SVSPP == 310, RPATH := 'RedCrab']
spp[SVSPP == 401, RPATH := 'AtlScallop']
spp[SVSPP == 502, RPATH := 'Illex']
spp[SVSPP == 503, RPATH := 'Loligo']
spp[SVSPP == 654, RPATH := 'RedDrum']

#Multispecies Groups
spp[SVSPP %in% c(379, 380), RPATH := 'Sturgeon']
spp[SVSPP %in% c(20, 24, 25, 27, 28, 368, 370, 371, 372, 924), RPATH := 'OtherSkates']
spp[SVSPP %in% c(33, 34), RPATH := 'RiverHerring']
spp[SVSPP %in% c(31, 43:46, 113, 132, 133, 208, 209, 211, 212, 423, 429, 430, 475, 
                 851, 856, 859, 865, 875, 889), RPATH := 'SmPelagics']
spp[SVSPP %in% c(30, 37, 38, 66, 68, 119, 120, 124, 127, 129, 130, 134, 138, 175, 
                 203:205, 265, 381, 382, 426:428, 463:471, 568:584, 693, 694, 701,
                 702, 743:746, 749, 750, 860, 876, 877, 894, 896, 898, 938),
    RPATH := 'OtherPelagics']
spp[SVSPP %in% c(39, 47, 51:59, 61, 90, 91:93, 111, 114, 126, 137, 144, 150, 210, 
                 213, 215, 221, 227:252, 260:262, 280, 281, 748, 822:824, 829, 
                 874, 883, 885, 887, 888, 891), 
    RPATH := 'Mesopelagics']
spp[SVSPP %in% c(98, 100, 110, 773:795, 853, 866, 873, 882), RPATH := 'OtherFlatfish']
spp[SVSPP %in% c(109, 117, 118, 796:799, 821, 825:827), RPATH := 'SmFlatfishes']
spp[SVSPP %in% c(431, 432, 441, 442), RPATH := 'Freshwater']
spp[SVSPP %in% c(140, 142, 146:149, 268, 274, 383, 385:389, 454, 455, 477:490, 
                 496:500, 523, 524, 526:559, 585:599, 616:620, 625:641, 643:653, 
                 655:692, 695:699, 725:733, 735:742, 751:770, 772, 820, 830:850, 
                 854, 855, 857, 858, 861:864, 867, 869:872, 878:881, 892, 895, 
                 897, 899, 900:902), RPATH := 'SouthernDemersals']
spp[SVSPP %in% c(403, 409), RPATH := 'Clams']
spp[SVSPP %in% c(501, 504:515, 519, 520), RPATH := 'OtherCephalopods']
spp[SVSPP %in% c(302:304, 308, 309, 311:315, 317, 318, 320:329, 339:343, 400, 402, 
                 404:408, 410:420, 516, 517, 518, 604), RPATH := 'Megabenthos']
spp[SVSPP %in% c(170, 319, 330:338, 344:349, 912), RPATH := 'Macrobenthos']
spp[SVSPP %in% c(3, 6:9, 11, 12, 14, 16, 17, 350:366, 602, 603, 925:937), 
    RPATH := 'Sharks']
spp[SVSPP %in% c(285:287, 291:299, 305, 307, 316, 323, 910, 911, 913, 914, 915),
    RPATH := 'OtherShrimps']
spp[SVSPP %in% c(4, 5, 18, 19, 21, 29, 270, 271, 272, 367, 373:378), RPATH := 'Rays']
spp[SVSPP %in% c(123, 125, 128, 700, 703:708, 747, 939:943), RPATH := 'LargePelagics']
spp[SVSPP %in% c(950:954), RPATH := 'Turtles']
spp[SVSPP %in% c(151, 562, 621:624), RPATH := 'Tilefish']
spp[(!is.na(SVSPP) & !SVSPP %in% c(0, 998)) & is.na(RPATH), RPATH := 'OtherDemersals']

#Assign NESPP3 codes that do not have an SVSPP
spp[NESPP3 %in% c(45, 63:69, 84, 104, 133, 223, 423, 426, 526), RPATH := 'Freshwater']
spp[NESPP3 %in% c(3, 18, 25, 52, 105, 165, 170, 214, 215, 221, 306:309),
    RPATH := 'OtherPelagics']
spp[NESPP3 == 98, RPATH := 'Mesopelagics']
spp[NESPP3 %in% c(141, 144, 197, 234, 236, 249, 271, 330, 334, 336, 381, 392, 400, 
                  401, 513, 517), RPATH := 'SouthernDemersals']
spp[NESPP3 %in% c(171, 371), RPATH := 'SmallPelagics']
spp[NESPP3 %in% c(218, 465), RPATH := 'HMS']
spp[NESPP3 %in% c(286:288, 670, 673), RPATH := 'Rays']
spp[NESPP3 %in% c(357, 359, 360, 475, 495, 497, 498, 501, 504), RPATH := 'Sharks']
spp[NESPP3 %in% c(365, 373, 378), RPATH := 'OtherSkates']
spp[NESPP3 %in% c(420, 421), RPATH := 'Sturgeon']
spp[NESPP3 == 447, RPATH := 'Tilefish']
spp[NESPP3 %in% c(688, 702, 715, 716, 743, 748, 760, 761, 763, 764, 774, 775, 
                  778, 789, 792, 798, 804:806, 823, 828, 899), RPATH := 'Macrobenthos']
spp[NESPP3 %in% c(713, 714, 796), RPATH := 'Megabenthos']
spp[NESPP3 %in% c(730, 731, 734, 735, 737, 738, 739), RPATH := 'OtherShrimp']
spp[NESPP3 %in% c(786, 803, 807), RPATH := 'OtherCephalopods']
spp[NESPP3 %in% c(808, 810, 811, 815, 816, 818), RPATH := 'Turtles']
spp[NESPP3 %in% c(817, 819:822, 824:827, 829:833), RPATH := 'Fauna']
spp[is.na(RPATH) & is.na(SVSPP), RPATH := 'OtherDemersals']

#Assign trash as fauna
spp[SVSPP == 998, RPATH := 'Fauna']

setkey(spp, SVSPP, NESPP3, ITISSPP)
spp <- unique(spp)
save(spp, file = file.path(out.dir, 'Species_codes.RData'))
