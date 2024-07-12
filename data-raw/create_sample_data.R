#' Create sample data fro vignettes
#'
#'
#'

channel <- dbutils::connect_to_database("server","user")
# pull raw data
raw <- survdat::get_survdat_data(channel,
                          getLengths = F,
                          conversion.factor = F)

# pull lengths
len <- survdat::get_survdat_data(channel,
                          getLengths = T,
                          conversion.factor = F)
# pull lengths with predicted weights
lenwgt <- survdat::get_survdat_data(channel,
                          getLengths = T,
                          getWeightLength = T,
                          conversion.factor = F)
# Pull bio data
bio <- survdat::get_survdat_data(channel,
                          getLengths = T,
                          getWeightLength = F,
                          conversion.factor = F,
                          getBio = T)

# Pull raw data with conversion factors
rawfact <- survdat::get_survdat_data(channel,
                                 getLengths = F,
                                 conversion.factor = T)

overwrite <- F
set.seed(143)
rowsToSample <- sample(1:nrow(raw$survdat),20)

# Generate sample data
# raw
sampleRaw <- raw$survdat |>
  dplyr::slice(rowsToSample) |>
  dplyr::as_tibble()

usethis::use_data(sampleRaw,overwrite = overwrite)

#lengths
sampleLengths <- len$survdat |>
  dplyr::slice(rowsToSample) |>
  dplyr::as_tibble()

usethis::use_data(sampleLengths,overwrite = overwrite)

# lengthweight
sampleLengthWeight <- lenwgt$survdat |>
  dplyr::slice(rowsToSample) |>
  dplyr::as_tibble()

usethis::use_data(sampleLengthWeight,overwrite = overwrite)

# BiologicalData
sampleBio <- bio$survdat |>
  dplyr::slice(rowsToSample) |>
  dplyr::as_tibble()

usethis::use_data(sampleBio,overwrite = overwrite)

# raw with conversion factor
sampleRawWithConversion <- rawfact$survdat |>
  dplyr::slice(rowsToSample) |>
  dplyr::as_tibble()

usethis::use_data(sampleRawWithConversion,overwrite = overwrite)
