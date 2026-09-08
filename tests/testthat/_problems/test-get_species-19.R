# Extracted from test-get_species.R:19

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "survdat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(dbutils)
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod","RGAMBLE")

# test -------------------------------------------------------------------------
area <- sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
res <- get_area(areaPolygon = area, areaDescription="STRATA")
expect_true(nrow(res$Area) > 0)
