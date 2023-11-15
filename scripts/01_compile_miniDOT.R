source("scripts/00_libraries.R")

#Compile miniDOT data for Sky, Loch, Fern

#sky pond database####
sky_minidot <- bind_rows((fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Sky/sky_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Sky/sky_3.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "3.5") %>%
    mutate(date_time = as_datetime(`date_time`)),
  fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Sky/sky_6", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
    mutate(date_time = as_datetime(`date_time`)))

#loch database####
loch_minidot <- bind_rows((fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Loch/loch_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Loch/loch_4", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
    mutate(date_time = as_datetime(`date_time`)))
#this is generating some NA values when converting unix to date_time. not sure why, will fix -AGK

#Check where the NAs are:
# loch_minidot %>% filter(if_any(everything(), is.na))
# Did not find any NAs with the function above so just overriding date_time with date_time



#fern database####
fern_minidot <- bind_rows((fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Fern/fern_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 
  fs::dir_ls("Data/Loch Vale/LVWS_data/miniDOT/raw/Fern/fern_5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "5") %>%
    mutate(date_time = as_datetime(`date_time`)))

