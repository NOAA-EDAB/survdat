# ==============================================================================
# Issue #59: Clam Data Strata Comparison Test
# ==============================================================================
library(data.table)
library(ggplot2)
library(maps)

# Load the entire package
devtools::load_all()

# 1. Setup and Load Functions
# ------------------------------------------------------------------------------
# Extract the old script from the main branch 
old_code <- system("git show main:R/get_survdat_clam_data.r", intern = TRUE)
writeLines(old_code, "data-raw/old_get_survdat_clam_data.r")

# Source the old function into a temporary environment so it doesn't conflict
# with the newly updated package version
tmp_env <- new.env()
sys.source("data-raw/old_get_survdat_clam_data.r", envir = tmp_env)
get_survdat_clam_data_old <- tmp_env$get_survdat_clam_data

# Prompt for username in the console
my_user <- readline(prompt = "Enter your Oracle username: ")

# Connect to the database
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", my_user)

# 2. Performance Benchmark
# ------------------------------------------------------------------------------
message("Performance Comparison:")
message("Old version took: ", round(t_old["elapsed"], 2), " seconds")
message("New version took: ", round(t_new["elapsed"], 2), " seconds")

# 3. Data Integrity Checks
# ------------------------------------------------------------------------------
message("\n--- Checking Row Counts ---")
if (nrow(old_data) == nrow(new_data)) {
  message("PASS: Row counts match perfectly (", nrow(old_data), " rows).")
} else {
  message("FAIL: Row counts differ! Old: ", nrow(old_data), " | New: ", nrow(new_data))
}

# Merge datasets for direct comparison. 
# (stripped leading zeros and turned 2018+ strings into NAs).
comp_dt <- merge(
  old_data[, .(CRUISE6, STATION, old_strat = STRATUM, SVSPP, LENGTH, 
               old_region = clam.region, old_mw = BIOMASS.MW)],
  new_data[, .(CRUISE6, STATION, new_strat = STRATUM, SVSPP, LENGTH, LAT, LON,
               new_region = clam.region, new_mw = BIOMASS.MW)],
  by = c("CRUISE6", "STATION", "SVSPP", "LENGTH"),
  all = TRUE
)

message("\n--- Checking for Unmatched Rows ---")
unmatched <- nrow(comp_dt[is.na(old_region) | is.na(new_region)])
if (unmatched == 0) {
  message("PASS: All rows merged successfully.")
} else {
  message("FAIL: Found ", unmatched, " unmatched rows.")
}

# 4. Region Translation Checks
# ------------------------------------------------------------------------------
message("\n--- Region Translation Matrix (Old vs New) ---")
# This shows how the old 7 regions map to the new South/GBK regions
print(table(Old = comp_dt$old_region, New = comp_dt$new_region, useNA = "ifany"))

# 5. Post-2017 Strata Check
# ------------------------------------------------------------------------------
message("\n--- Checking 2018+ Region Assignments ---")
comp_dt[, year := floor(as.numeric(CRUISE6) / 100)]
recent_data <- comp_dt[year >= 2018]

print(table(Year = recent_data$year, New_Region = recent_data$new_region, useNA = "ifany"))


# 6. Explicit 2018+ Missing Data Exploration (Table)
# ------------------------------------------------------------------------------
message("\n--- 2018+ Data Dropped by Old Script ---")
missing_summary <- recent_data[, .(
  Total_Tows_New = .N,
  Dropped_By_Old = sum(is.na(old_region)),
  Percent_Lost = round((sum(is.na(old_region)) / .N) * 100, 1)
), by = year][order(year)]

print(missing_summary)


# 7. Mapping the Missing Data
# ------------------------------------------------------------------------------
message("\n--- Generating Spatial Map of Dropped 2018+ Tows ---")

# Reduce to unique stations to avoid over-plotting multiple length records per tow
map_dt <- unique(recent_data[, .(CRUISE6, STATION, LAT, LON, 
                                 Status = data.table::fifelse(is.na(old_region), 
                                                              "Dropped by Old Script", 
                                                              "Kept by Old Script"))])

# Pull basic coastlines for context
coast <- map_data("state")

p <- ggplot() +
  # Draw the coastline
  geom_polygon(data = coast, aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "white") +
  # Plot the survey tows
  geom_point(data = map_dt, aes(x = LON, y = LAT, color = Status), 
             size = 1.5, alpha = 0.7) +
  # Focus the map on the US East Coast / Northeast Shelf
  coord_quickmap(xlim = c(-77, -65), ylim = c(36, 43)) +
  scale_color_manual(values = c("Dropped by Old Script" = "#D55E00", 
                                "Kept by Old Script" = "#0072B2")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Missing Clam Survey Tows (2018-Present)",
    subtitle = "Comparing data retention between the old vs. new region-mapping logic",
    x = "Longitude", 
    y = "Latitude", 
    color = "Tow Status"
  )

# Display the plot in your R session
print(p)

# Save the plot to the data-raw folder to attach to your pull request
ggsave("data-raw/i59_missing_2018_tows_map.png", p, width = 8, height = 6, bg = "white")
message("Map saved to 'data-raw/i59_missing_2018_tows_map.png'")

# 8. Identifying the Exact Year of the Strata Transition
# ------------------------------------------------------------------------------
message("\n--- Identifying the Exact Year of the Strata Transition ---")
# Look at raw STRATUM codes between 2015 and 2019 to see exactly when the format shifts
new_data[, year := floor(as.numeric(CRUISE6) / 100)]
transition_data <- new_data[year >= 2015 & year <= 2019]

# Flag the strata format based on its value/length
transition_data[, stratum_format := data.table::fifelse(
  as.numeric(STRATUM) <= 12, "New Format (1-12)", "Old Format (6000+)"
)]

# Create a cross-tab to pinpoint the shift
transition_summary <- transition_data[, .N, by = .(year, stratum_format)][order(year)]
transition_table <- data.table::dcast(transition_summary, year ~ stratum_format, value.var = "N", fill = 0)

print(transition_table)

# Optional cleanup: remove the temporary old script
unlink("data-raw/old_get_survdat_clam_data.r")