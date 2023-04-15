# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(tidycensus)


# Load Data ---------------------------------------------------------------

# IWU

data_folder = here("data", "raw")
(data_files = list.files(data_folder, full.names = T, pattern = "(sw|gwa|gwd)_20[0-9]{2}"))

iwu_df_list = map(data_files, read_csv, col_types = "cdddddddddddddddddddd", show_col_types = F)

# USGS
usgs_file = list.files(data_folder, pattern = "usco.+\\.csv$", full.names = T)

usgs = read_csv(usgs_file, skip = 1, na = "--") |> 
  clean_names()
cli::cli_progress_message("IWU and USGS data loaded")

# Pivot Data --------------------------------------------------------------


# Long: One row containing one measure per crop, county, year
(iwu_long = iwu_df_list |> 
  map(mutate, GEOID = str_pad(GEOID, pad = "0", width = 5, side = "left")) |> 
  # Pivot each df to rows of county-level crop measures
  map(pivot_longer, -GEOID, 
      names_to = c("measure", "crop", "year"), 
      names_sep = "\\.", 
      names_transform = list(year = ~as.integer(.))) |> 
  # Combine these dfs as rows
  list_rbind()) |> 
  glimpse()



# Wide: one row containing all measures per crop, county, year
(iwu_wide = iwu_long |> 
  # Pivot the measure names back out
  pivot_wider(names_from = measure, values_from = value) |> 
  # Reorder measures to match table in paper
  # https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2022WR032804#wrcr26385-tbl-0002
  select(year, GEOID, crop, sw, gwa, gwd)) |> 
  glimpse()

# USGS

# Function
#' Convert mgal/day to km^3
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mgal_to_km3 = function(x){
  # Multiple by days to get gallons/year
  mgal_year = x*365.25
  # Multiply by 0.0000037854118 because this is the conversion factor for mgal to km^3?
  # https://www.unitjuggler.com/convert-volume-from-gallonus-to-km3.html?val=43117815000000
  mgal_year*0.000003785411784
}

usgs_iwu_long = usgs |> 
  # Select groundwater, surface, and total freshwater use measures
  select(state:year, contains("_w_fr_to"), contains("_wgw_fr"),
         contains("_wsw_fr"), contains("_rec_ww")) |> 
  # Rename the measures
  rename_with(\(x) str_replace_all(x, c("(?<=_)w_fr_to" = "total",
                                        "(?<=_)wgw_fr" = "gwa",
                                        "(?<=_)wsw_fr" = "sw",
                                        "(?<=_)rec_ww" = "reclaimed",
                                        "(?<=_)c_us_fr" = "consumption")),
              -c(state:year))

usgs_iwu_wide = usgs_iwu_long |> 
    # Pivot the use categories to a column and separate out the different measures
    pivot_longer(cols = contains("_"), names_to = c("category", ".value"), names_sep = "_",
                 names_transform = list(category = \(x) factor(x, 
                                                               levels = c("ps", "do", "in", "ir", 
                                                                          "ic", "ig", "li", "aq", 
                                                                          "mi", "pt", "po", 
                                                                          "pc", "to"),
                                                               labels = c("Public Supply", "Domestic", "Industrial", "Irrigation", 
                                                                          "Crop Irrigation", "Golf Irrigation", "Livestock", "Aquaculture", 
                                                                          "Mining", "Thermoelectric", "Thermoelectric (Once-through)", 
                                                                          "Thermoelectric (Recirculating)", "Total")))) |> 
  # Convert estimates
  mutate(across(-c(state:category), mgal_to_km3))


# Check table -------------------------------------------------------------

iwu_wide |> 
  # Sum measures by year and crop
  group_by(year, crop) |> 
  summarise(across(where(is_double),sum)) |> 
  # Calculate crop annual avg measures
  group_by(crop) |> 
  summarise(across(where(is_double), \(x) mean(x, na.rm=T)))|> 
  mutate(across(where(is_double), \(x) round(x, 2)))


# Join State Info ---------------------------------------------------------

data("fips_codes")
fips_clean = fips_codes |> 
  # Combine codes to create FIPS code
  unite(col = fips, c(state_code, county_code), sep = "")

iwu_long_st = iwu_long |> 
  left_join(fips_clean, by = c("GEOID" = "fips"))

iwu_wide_st = iwu_wide |> 
  left_join(fips_clean, by = c("GEOID" = "fips"))


# Write Output ------------------------------------------------------------


write_csv(iwu_long_st, here("data", "processed", "iwu_full_long.csv"))
write_csv(iwu_wide_st, here("data", "processed", "iwu_full_wide.csv"))

write_csv(usgs_iwu_wide, here("data", "processed", "usgs_iwu_wide.csv"))
