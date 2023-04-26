#' Test to see if all simple get functions return data
#'
#' Requires a connection object
#'

library(survdat)
message("Getting conversion factors data")
a <- get_conversion_factors(channel)
if(nrow(a$data) != 53) {
  stop("get_conversion_factors Test Failed")
}

message("Getting cruise purpose data")
a <- get_cruise_purpose(channel)
if(nrow(a$data) !=56) {
  stop("get_cruise_purpose Test Failed")
}

message("Getting length_weight data")
a <- get_length_weight(channel)
if(nrow(a$data) != 424) {
  stop("get_length_weight Test Failed")
}

message("Getting maturity data")
a <- get_maturity(channel)
if(nrow(a$data) != 18) {
  stop("get_maturity Test Failed")
}

message("Getting sex data")
a <- get_sex(channel)
if(nrow(a$data) != 8) {
  stop("get_sex Test Failed")
}

message("Getting sex_fscs data")
a <- get_sex_fscs(channel)
if(nrow(a$data) != 4) {
  stop("get_sex_fscs Test Failed")
}

message("Getting species data")
a <- get_species(channel)
if(nrow(a$data) != 2047) {
  stop("get_species Test Failed")
}

message("Getting strata data")
a <- get_strata(channel)
if(nrow(a$data) != 369) {
  stop("get_strata Test Failed")
}

message("Getting vessel data")
a <- get_vessel(channel)
if(nrow(a$data) != 288) {
  stop("get_vessel Test Failed")
}


message("All tests passed !!!!")
