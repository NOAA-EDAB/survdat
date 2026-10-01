library(survdat)

west_longitude <- survdat:::clam_west_longitude

lon <- west_longitude(c(-70, -68))
side_47 <- (lon - 69.23) * (41 - 40) - (c(40, 40) - 40) * (69.03 - 69.23)
stopifnot(identical(ifelse(side_47 > 0, "471", "472"), c("471", "472")))

lon <- west_longitude(c(-67.2, -66.5))
side_73 <- (lon - 66.8) *
  (41.9 - 41.35) -
  (c(41.5, 41.5) - 41.35) * (67.5 - 66.8)
stopifnot(identical(ifelse(side_73 > 0, "73", "74"), c("73", "74")))

lon <- west_longitude(-72.2)
side_quahog <- (lon - 72) * (40.2 - 39.3) - (40 - 39.3) * (73.75 - 72)
stopifnot(side_quahog < 0)
