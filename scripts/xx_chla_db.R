source("scripts/00_libraries.R")

# trim chla db for exporting and write to csv file
fern_chla <- read.csv("Data/Loch Vale/water_chemistry/chlorophyll/lvws_chlorophyll.csv", header = TRUE) %>%
  filter(lake == "FERN") %>%
  select(1:13) %>%
  mutate(date_sample = mdy(date_sample)) %>%
  filter(date_sample >= "2023-01-01") %>%
  rename(chla_final = 13) %>%
  write_csv("fern_chla.csv", col_names = TRUE)

fern_chla_all <- read.csv("Data/Loch Vale/water_chemistry/chlorophyll/lvws_chlorophyll.csv", header = TRUE) %>%
  filter(lake == "FERN") %>%
  select(1:13) %>%
  mutate(date_sample = mdy(date_sample)) %>%
  rename(chla_final = 13) %>%
  write_csv("fern_chla_all.csv", col_names = TRUE)

fchla_db <- fern_chla_all %>%
  mutate(sample_ID = as.factor(sample_ID)) %>%
  mutate(chla_final = as.numeric(chla_final))

fchla_db %>%
  ggplot(aes(x = date_sample, y = chla_final)) +
  geom_point() +
  facet_grid(~ sample_ID)






























