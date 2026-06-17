#' Explore how pervasive BIOMASS = 0 is in the data set when ABUNDANCE > 0
#'
#' This would be problematic for stratified means since we;d be underreporting biomass and
#' lowering the stratified mean
#'
#'
#'

channel <- dbutils::connect_to_database("server", "user")

# pull survey data
a <- survdat::get_survdat_data(channel, getLengths = FALSE)


# find rows in which biomass is zero
zeros <- a$survdat |>
  dplyr::filter(BIOMASS < .0001 & ABUNDANCE > 0)

# plot frequency of records over time
p1 <- zeros |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = n)) +
  ggplot2::ggtitle("BIOMASS(EXPCATCHWT) = 0 and ABUNDANCE(EXPCATCHNUM) > 0") +
  ggplot2::ylab("Number of records")

# grab species code definitions
sp <- survdat::get_species(channel)$data |>
  dplyr::select(SVSPP, SCINAME, COMNAME) |>
  dplyr::distinct()

# plot table of species with number of records attributed to them
zeros |>
  dplyr::group_by(SVSPP) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(desc(n)) |>
  dplyr::left_join(sp, by = "SVSPP") |>
  DT::datatable()

p2 <- zeros |>
  dplyr::group_by(SVSPP) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(desc(n)) |>
  dplyr::slice(1:10) |>
  dplyr::left_join(sp, by = "SVSPP") |>
  dplyr::mutate(NAME_SVSPP = paste0(COMNAME, " (", SVSPP, ")")) |>
  ggplot2::ggplot(ggplot2::aes(x = NAME_SVSPP, y = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::coord_flip() +
  ggplot2::ylab("Number of records") +
  ggplot2::xlab("") +
  ggplot2::ggtitle("Top 10 fish by number of unweighed tows")

p3 <- zeros |>
  dplyr::group_by(SVSPP) |>
  dplyr::summarise(n = sum(ABUNDANCE), .groups = "drop") |>
  dplyr::arrange(desc(n)) |>
  dplyr::slice(1:10) |>
  dplyr::left_join(sp, by = "SVSPP") |>
  dplyr::mutate(NAME_SVSPP = paste0(COMNAME, " (", SVSPP, ")")) |>
  ggplot2::ggplot(ggplot2::aes(x = NAME_SVSPP, y = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::coord_flip() +
  ggplot2::ylab("Number of fish") +
  ggplot2::xlab("") +
  ggplot2::ggtitle("Top 10 fish by number of unweighed fish")

ggplot2::ggsave(here::here("data-raw/explore/biomass1.png"), p1)
ggplot2::ggsave(here::here("data-raw/explore/biomass2.png"), p2)
ggplot2::ggsave(here::here("data-raw/explore/biomass3.png"), p3)


### Now look at lengths of these fish to see if they are small/negligible weight
# as suggested by survey

a2 <- survdat::get_survdat_data(channel, getLengths = TRUE)

# find rows in which biomass is zero
zeros2 <- a2$survdat |>
  dplyr::filter(BIOMASS < .0001 & ABUNDANCE > 0)
dim(zeros2)

zeros2 |>
  dplyr::filter(is.na(LENGTH)) |>
  nrow()
## Lengths of missing species
p4 <- zeros2 |>
  dplyr::filter(!is.na(LENGTH) & LENGTH < 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = LENGTH), binwidth = 0.2) +
  ggplot2::ggtitle("Lengths of species without weights")

p5 <- zeros2 |>
  dplyr::filter(!is.na(LENGTH), SVSPP == 15) |>
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = LENGTH), binwidth = 0.2) +
  ggplot2::ggtitle("Lengths of Spiny Dogfish without weights")


ggplot2::ggsave(here::here("data-raw/explore/biomass4.png"), p4)
ggplot2::ggsave(here::here("data-raw/explore/biomass5.png"), p5)
