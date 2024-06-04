source("scripts/00_libraries.R")
source("scripts/05_miniDOT.R")

# this script is meant to be used to process miniDOT data for export

# filter loch data to for August 2021 - exporting for Janice (?)
loch_082021 <- loch_minidot %>%
  filter(year == 2021) %>%
  filter(month == 8) %>%
  rename(do_mg = do_obs) 

# this file path is not currently correct. 
write_csv(loch_082021, "Data/Loch Vale/miniDOT/export/Loch_Aug2021.csv", col_names = TRUE)

# write Fern csv for Mollie
write_csv(fern_minidot, "Data/LVWS/05_miniDOT/FERN/export/fern_minidot.csv", col_names = TRUE)





























