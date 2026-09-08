# Extracted from test-get_species.R:8

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "survdat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(dbutils)
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod","RGAMBLE")

# test -------------------------------------------------------------------------
res <- get_species(channel,species = 73)
