#' Create survdat::Wigley_LW data object
#' Dataset read in as csv ("tech_memo_parameters_table_format.csv") in original condition calculation here (https://github.com/Laurels1/Condition/blob/master/R/RelConditionEPU.R

create_Wigley_LW <- function() {
  # Read in the CSV file
  Wigley_LW <- read.csv(
    here::here('data/Wigley_LW.csv'),
    stringsAsFactors = FALSE
  )

  usethis::use_data(Wigley_LW, overwrite = TRUE)

  return(Wigley_LW)
}
