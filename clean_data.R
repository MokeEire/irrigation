# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# Load Data ---------------------------------------------------------------


data_folder = here("data", "raw")
(data_files = list.files(data_folder, full.names = T))

iwu_df_list = map(data_files, read_csv, col_types = "cdddddddddddddddddddd", show_col_types = F)


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
(iwu_wide = irrigation_long |> 
  # Pivot the measure names back out
  pivot_wider(names_from = measure, values_from = value) |> 
  # Reorder measures to match table in paper
  # https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2022WR032804#wrcr26385-tbl-0002
  select(year, GEOID, crop, sw, gwa, gwd)) |> 
  glimpse()


# Write Output ------------------------------------------------------------


write_csv(iwu_long, here("data", "processed", "iwu_full_long.csv"))
write_csv(iwu_wide, here("data", "processed", "iwu_full_wide.csv"))
