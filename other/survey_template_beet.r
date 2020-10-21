#New Survey Template for use with Survdat package
#SML

#-------------------------------------------------------------------------------
#Required packages
#May need to download Survdat package from GitHub
#devtools::install_github('slucey/RSurvey/Survdat')
library(data.table); library(rgdal); library(magrittr)#library(Survdat)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
library(magrittr)
#Grab survdat.r
survdatData <- readRDS(here::here("survdat.RDS"))

########################################################################## OLD
#Grab strata
strata <- rgdal::readOGR(system.file("extdata","EPU.shp",package="survdat"), 'EPU')
strat.area <- survdat::getarea(strata, 'EPU')
#data.table::setnames(strat.area, 'EPU', 'STRATUM')

#Post stratify data if necessary
survdat.epu <- survdat::poststrat(survdatData$survdat, strata)
data.table::setnames(survdat.epu, 'newstrata', 'EPU')
survdat <- survdat.epu

fall.GOM <- survdat[SEASON == "SPRING" & EPU %in% c("GB","MAB"), ]

#Run stratification prep
GOM.prep <- survdat::stratprep(fall.GOM, strat.area, strat.col = 'EPU',
                      area.col = 'Area')

#Note: The function will merge aggregated groups for you
lob.mean <- survdat::stratmean(GOM.prep, groups = 301:310, group.col = 'SVSPP',
                      strat.col = 'EPU')

#Calculate total biomass/abundance estimates
total.biomass <- survdat::sweptarea(GOM.prep, lob.mean, strat.col = 'EPU',
                           area.col = 'Area')


################################################################### NEW
library(magrittr)
#Grab survdat.r
survdatData <- readRDS(here::here("survdat.RDS"))


strataN <- sf::st_read(dsn = system.file("extdata","strata.shp",package="survdat"))
strat.areaN <- survdat::get_area(strataN,"STRATA")
survdat.epuN <- survdat::post_strat(survdatData$survdat, strataN, strata.col="STRATA")
fall.GOM <- survdat.epuN[SEASON == 'FALL' & STRATA %in% c(1220, 1240, 1260:1290,
                                                      1360:1400), ]
GOM.prep2 <- survdat::strat_prep(fall.GOM, strat.areaN, strat.col = 'STRATA')

lob.mean2 <- survdat::strat_mean(GOM.prep2, groups = 301, group.col = 'SVSPP',
                                 strat.col = 'REGION')


strataN <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
strat.areaN <- survdat::get_area(strataN,"EPU")
survdat.epuN <- survdat::post_strat(survdatData$survdat, strataN, strata.col="EPU")

fall.GOM <- survdat.epuN[SEASON == "FALL" & EPU == "GB", ]

#Run stratification prep
GOM.prep2 <- survdat::strat_prep(fall.GOM, strat.areaN, strat.col = 'EPU')

lob.mean2 <- survdat::strat_mean(GOM.prep2, groups = 301, group.col = 'SVSPP',
                      strat.col = 'REGION')

#Calculate total biomass/abundance estimates
total.biomass2 <- survdat::swept_area(GOM.prep2, lob.mean2, strat.col = 'REGION')

#######################################################################





#Subset by season/ strata set
# fall.GOM <- survdat[SEASON == 'FALL' & STRATUM %in% c(1220, 1240, 1260:1290,
#                                                       1360:1400), ]




#Calculate stratified means - can do multiple species at once (i.e. groups =
#c(72, 73, 75))
#You can also aggregate species here
#GOM.prep[SVSPP %in% 72:76, Group := 'Gadid']
#then use groups = 'Gadid' and group.col = 'Group' in stratmean
#Note: The function will merge aggregated groups for you

# codes
# spiny dogfish = 15
# winter skate = 23
# atlantic herring = 32
# atlantic cod = 73
# haddock = 74
# winter flounder = 106
# yellowtail flounder = 105
# mackerel = 121
# silver hake = 72
# goosefish = 197
# HYDRA species SVSPP codes
hydraSpecies <- data.frame(SVSPP = c(15,23,32,72,73,74,105,106,121,197),
                           species_name = c("Spiny_dog","Winter_skate","Atlantic_herring","Silver_hake",
                                            "Atlantic_cod","Haddock","Yellowtail_flounder","Winter_flounder",
                                            "Mackerel","Goosefish"))

species.mean <- stratmean(GOM.prep, groups = hydraSpecies$SVSPP, group.col = 'SVSPP',
                      strat.col = 'EPU')

#### #Calculate total biomass/abundance estimates
load(here::here("beet_stuff","Species_codes.RData")) # load in catchabilites from EMAcs study
emax <- spp %>% dplyr::filter(SVSPP %in% hydraSpecies$SVSPP)
# take mean of speing and fall q's
emax <- emax %>% dplyr::distinct(SVSPP,COMNAME,Fall.q,Spring.q) %>% dplyr::mutate(meanq=(Fall.q+Spring.q)/2)
emax <- emax %>% dplyr::distinct(SVSPP,meanq)
names(emax) <- c("SVSPP","q")
emax$q[emax$SVSPP==197] <- 0.01# mean(c(.01766,.007852)) # goosefish SAW q's
emax$q[emax$SVSPP==105] <- 0.1 # made up q for VAST workshop (yellowtail)
emax$q[emax$SVSPP==121] <- 0.006 # made up q for VAST workshop (yellowtail)


#emax <-  dplyr::left_join(emax,hydraSpecies,by="SVSPP")
total.biomass <- sweptarea(GOM.prep, species.mean, q= emax, strat.col = 'EPU', area.col = 'Area')

# convert to metric ton out put to rda
hydraDataSurvey <- total.biomass %>% dplyr::mutate(metric_ton = tot.biomass/1000) %>% dplyr::select(YEAR,SVSPP,metric_ton)
hydraDataSurvey <- dplyr::left_join(hydraDataSurvey,hydraSpecies,by="SVSPP")
hydraDataSurvey <- tibble::as_tibble(hydraDataSurvey)

#saveRDS(hydraDataSurvey,file = here::here("beet_stuff","hydraDataSurvey_adjustedq.rds"))

#Output results either to a flat .csv file or .RData set
#write.csv(total.biomass, file = file.path(out.dir, 'Lobster_GoM.csv'), row.names = F)
#save(     total.biomass, file = file.path(out.dir, 'Lobster_GOM.RData'))
