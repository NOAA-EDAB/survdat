#' Key of survey strata and corresponding EPUs
#'
#' Strata to EPU conversions derived from SOE_Workflows repository (https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/main/R/aggregate_biomass.r#L118)
#'
#' @format A data frame with n rows and m variables
#'
#'
#'
#'
"EPUstrata"

# Define vectors containing strata values for each EPU
MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS <- c(1300:1352, 3840:3990)

EPUstrata <- data.frame(
  STRATUM = c(MAB, GB, GOM, SS),
  EPU = rep(
    c("MAB", "GB", "GOM", "SS"),
    times = c(length(MAB), length(GB), length(GOM), length(SS))
  )
)

usethis::use_data(EPUstrata, overwrite = TRUE)
