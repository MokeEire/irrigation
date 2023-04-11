
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# Load Data ---------------------------------------------------------------

data_folder = here("data", "raw")
(data_files = list.files(data_folder, full.names = T))

iwu_df_list = map(data_files, read_csv, col_types = "cnnnnnnnnnnnnnnnnnnnn", show_col_types = F)


# Pivot Data --------------------------------------------------------------

iwu_df_pivot = iwu_df_list |> 
  map(mutate, GEOID = str_pad(GEOID, pad = "0", width = 5, side = "left")) |> 
  # Pivot each df to rows of county-level crop measures
  map(pivot_longer, -GEOID, 
      names_to = c("measure", "crop", "year"), 
      names_sep = "\\.", 
      names_transform = list(year = ~as.integer(.))) |> 
  # Combine these dfs as rows
  list_rbind() |> 
  # Pivot the measure names back out
  pivot_wider(names_from = measure, values_from = value) |> 
  # Reorder cols
  select(crop, year, GEOID, sw, gwa, gwd)

summary(iwu_df_pivot)

write_csv(iwu_df_pivot, here("data", "processed", "iwu_2008_2020_clean.csv"))
