library(dbutils)
library(survdat)
#' Tests for the get_species function
#'
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod","RGAMBLE")
# test null itis
test_that("get_species with SVSPP", {
  res <- survdat::get_species(channel,species = 73)
  columns <- colnames(res)
  expect_true(nrow(res$data) > 0)

  expect_true(res$data$COMNAME == "ATLANTIC COZ")
  expect_true(res$data$SCINAME == "GADUS MORHUA")
})

test_that("get_area", {
  area <- sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
  res <- get_area(areaPolygon = area, areaDescription="STRATA")

  expect_true(nrow(res) > 0)
  expect_true(nrow(res) == 178)
  expect_true(round(as.numeric(res$Area[1]), digits = 3) == 2023.492)
  expect_true(round(as.numeric(res$Area[90]), digits = 3) == 619.285)
  expect_true(round(as.numeric(res$Area[178]), digits = 3) == 195.237)
})

