source("scripts/00_libraries.R")

# trim chla db for exporting and write to csv file
fern_chla <- read.csv("Data/Loch Vale/water_chemistry/chlorophyll/lvws_chlorophyll.csv", header = TRUE) %>%
  filter(lake == "FERN") %>%
  select(1:13) %>%
  mutate(date_sample = mdy(date_sample)) %>%
  filter(date_sample >= "2023-01-01") %>%
  rename(chla_final = 13) %>%
  write_csv("fern_chla.csv", col_names = TRUE)




































