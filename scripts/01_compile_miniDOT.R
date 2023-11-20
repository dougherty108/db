source("scripts/00_libraries.R")

#Compile miniDOT data for Sky, Loch, Fern

#sky pond database####
#raw files first, manually calculating do_sat
sky_raw <- bind_rows((fs::dir_ls("Data/Loch Vale/miniDOT/raw/Sky/sky_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/miniDOT/raw/Sky/sky_3.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "3.5") %>%
    mutate(date_time = as_datetime(`date_time`)),
  fs::dir_ls("Data/Loch Vale/miniDOT/raw/Sky/sky_6", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
    mutate(date_time = as_datetime(`date_time`))) %>%
  mutate(salinity = 0,
         do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66))#last term is atm pressure
#Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html

#concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
#have to convert date formatting in order to combine with dataframe of raw files
sky_concat <- bind_rows(read.table("Data/Loch Vale/miniDOT/concat/Sky/2016_17_SkyLH/Sky_6.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
    mutate(date_time = as_datetime(`date_time`)), 
  read.table("Data/Loch Vale/miniDOT/concat/Sky/2016_17_SkyLS/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)), 
  read.table("Data/Loch Vale/miniDOT/concat/Sky/2017_18_SkyLH/sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
    mutate(date_time = as_datetime(`date_time`)),
  read.table("Data/Loch Vale/miniDOT/concat/Sky/2017_18_SkyLS/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)))
# 'corrected' hypolimnion depth to 6m. will look through field notebooks and fix later -AGK

#build database
sky_minidot <- bind_rows(sky_raw, sky_concat)

#loch database####
#raw files first, manually concatenating do_sat
loch_raw <- bind_rows((fs::dir_ls("Data/Loch Vale/miniDOT/raw/Loch/loch_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/miniDOT/raw/Loch/loch_4", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
    mutate(date_time = as_datetime(`date_time`))) %>%
  mutate(salinity = 0,
         do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)) #last term is atm pressure
#Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html

#this is generating some NA values when converting unix to date_time. not sure why, will fix -AGK

#Check where the NAs are:
# loch_minidot %>% filter(if_any(everything(), is.na))
# Did not find any NAs with the function above so just overriding date_time with date_time

#concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
#have to convert date formatting in order to combine with dataframe of raw files
loch_concat <- bind_rows(read.table("Data/Loch Vale/miniDOT/concat/Loch/2016_17_LochLH/Loch_4.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
    mutate(date_time = as_datetime(`date_time`)), 
  read.table("Data/Loch Vale/miniDOT/concat/Loch/2016_17_LochLS/Loch_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)), 
  read.table("Data/Loch Vale/miniDOT/concat/Loch/2017_18_LochLH/Loch_hypo_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
    mutate(date_time = as_datetime(`date_time`)),
  read.table("Data/Loch Vale/miniDOT/concat/Loch/2017_18_LochLS/Loch_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)))
#adjusted hypo depth to 4m, will fix after checking field notebooks -AGK

#build database
loch_minidot <- bind_rows(loch_raw, loch_concat)


#fern database####
fern_minidot <- bind_rows((fs::dir_ls("Data/Loch Vale/miniDOT/raw/Fern/fern_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/miniDOT/raw/Fern/fern_5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "5") %>%
    mutate(date_time = as_datetime(`date_time`)))%>%
  mutate(salinity = 0,
         do_sat = 100 * do_obs/oxySol(temp, salinity, 0.7)) #last term is atm pressure
#Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html

