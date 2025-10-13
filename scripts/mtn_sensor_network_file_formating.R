
# FIRST, define the path to your shortcut of the MLL data folder
# data_path = "~/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data"
data_path = "~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/sensor_db/data" #Bella's
lake = "SKY"
water_year = "2024"

# call function to load specific year of data
doObs = get_miniDOT(data_path, lake, water_year)

sky_doObs <- combined_data_clean %>%
  filter(lake_id == "SKY") %>%
  mutate(waterYear = calcWaterYear(date_time)) %>%
  filter(waterYear == 2024) %>%
  mutate(
    flag = case_when(
      date_time >= as.POSIXct("2024-06-25 08:30:00", tz = "America/Denver") &
        date_time <= as.POSIXct("2024-06-25 14:25:00", tz = "America/Denver") ~ "above water",
      
      date_time > as.POSIXct("2024-09-24 11:00:00", tz = "America/Denver") ~ "above water",
      
      TRUE ~ "under water"
    )
  )



# Graph first raw data
sky_doObs %>%
  filter(flag != "above water") %>%
  filter(waterYear == "2024") %>% # select which water year you are interested in here
  #mutate(flag = case_when(date_time > "2024-06-25 08:30:00" & date_time < "2024-06-25 12:30:00" ~ "above water",
  #                        date_time > "2024-09-24 11:00:00" & date_time < "2024-09-30 00:00:00" ~ "above water",
  #                        
  #                        TRUE ~ "under water"))
  mutate(flag = case_when(temp >= 13.0 ~ "above water", 
                          TRUE ~ "under water", 
                          temp_diff > 0.5 ~ "above water", 
                          temp_diff < 0.49 ~ "under water"
  )) # different way of flagging above / below water. If we do it by temperature 
    # (assuming a given water body is never above 15 degC), then it removes the need for looking into metadata too much

# Graph first raw data
# Look for points where the buoy was above water. Need to check buoy sheets to confirm

doObs %>%
  filter(!flag == "above water") %>% # remove data that is flagged as above water
  ggplot(aes(x=date_time, y=temp, color=factor(depth_from_top)))+
  geom_point(alpha=0.2) + 
  theme_minimal()

# To pivot wider and have all the values show up next to each other, need to round down our datetimes
# Go to the nearest 5 minutes
WY_miniDOT_temperature <- doObs %>%
  filter(!flag == "above water") %>%
  ungroup() %>%
  select(date_time, temp, depth_from_top) %>%
  group_by(depth_from_top) %>%
  mutate(date_time_new = floor_date(date_time, "5 mins")) %>%
  select(-date_time) %>%
  pivot_wider(names_from = c(depth_from_top),
              values_from = temp,
              names_prefix = "wtr_",
              values_fn = mean)

# Convert back to long and graph to make sure nothing broke..
head(sky_WY2023_miniDOT_temperature)
sky_WY2023_miniDOT_temperature %>%
  pivot_longer(wtr_6.5:wtr_2.5) %>%

head(WY_miniDOT_temperature)
WY_miniDOT_temperature %>%
  pivot_longer(wtr6.5:wtr2.5) %>% # may need to change wtr columns here depending on what data is available
  ggplot(aes(x=date_time_new, y=value, color=name)) +
  geom_point() + 
  theme_minimal()

# Beautiful!

WY_miniDOT_DO <- doObs %>%
  filter(!flag == "above water") %>%
  ungroup() %>%
  select(date_time, do_obs, depth_from_top) %>%
  group_by(depth_from_top) %>%
  mutate(date_time_new = floor_date(date_time, "30 mins")) %>%
  select(-date_time) %>%
  pivot_wider(names_from = c(depth_from_top),
              values_from = do_obs,
              names_prefix = "doobs_",
              values_fn = mean)

WY_miniDOT_DO %>%
  pivot_longer(-date_time_new) %>%
  ggplot(aes(x=date_time_new, y=value, color=name))+
  geom_point() + 
  theme_minimal()


WY_miniDOT_DO %>%
  pivot_longer(-date_time_new) %>%
  mutate(month=month(date_time_new)) %>%
  filter(month=="4" & name=="doobs_2.5") %>%
  #filter(month=="4" & name=="doObs2.5") %>% # remove data from a specific month for some reason
  ggplot(aes(x=date_time_new, y=value, color=name))+
  geom_point()

sky_WY2023_miniDOT_DO <- sky_WY2023_miniDOT_DO %>%
  rename(datetime=date_time_new) %>%
  mutate(local_TZ="America/Denver",
         daylight_savings="yes") %>%
  relocate(doobs_2, .after = datetime) %>%
  relocate(doobs_2.5, .after = doobs_2) %>%
  relocate(doobs_6.5, .after = everything()) 
write_csv(sky_WY2023_miniDOT_DO, "data_export/mtn_sensor_network/Oleksy_SouthernRockies_WY2024_SkyPond_DO.txt")



# Now graph the HOBO data for the same time period
source("functions/04_HOBO.R")
# change the below directory to your local filepath
main_dir <- file.path("~/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/Sensors/HOBO") 
all_HOBO <- compile_HOBO_data(filepath = main_dir)
site_HOBO <- all_HOBO %>%
  filter(lake_ID == "SKY") %>% # enter your lake ID here
  mutate(waterYear = calcWaterYear(date_time)) %>%
  filter(waterYear == "2024") %>%
  mutate(flag = case_when(date_time > as.POSIXct("2024-06-23 08:30:00", tz = "America/Denver") & date_time < as.POSIXct("2024-06-25 12:30:00", tz = "America/Denver") ~ "above water",
                          date_time > as.POSIXct("2024-09-23 11:10:00", tz = "America/Denver") ~ "above water",
                          TRUE ~ "under water"))

# closer look
#HOBO_fewdays <- site_HOBO %>%
#  filter(date_time > "2024-09-24")

#Filter for only in water measurements (flagging requires that metadata exist for buoy deployment somewhere)
# and filter out any depth from top values that are "0". Plot. 
site_HOBO %>%
  filter(!flag == "above water") %>%
  filter(!depth_from_top %in% c("0")) %>%
  ggplot(aes(x=date_time, y=temperature_C, color=factor(depth_from_top)))+
  geom_point(alpha=0.2)

# Want to keep depth_from_top 1 only after ice off...
site_HOBO %>%
  filter(!flag == "above water") %>%
  filter(!depth_from_top %in% c(0,1) | (depth_from_top %in% c(0,1) & date_time > as.POSIXct("2024-06-25"))) %>%
  ggplot(aes(x=date_time, y=temperature_C, color=factor(depth_from_top)))+
  geom_point(alpha=0.2)


site_HOBO_all <- sky_HOBO %>%
  filter(!flag == "above water") %>%
  filter(!depth_from_top %in% c(0,1) |
           (depth_from_top %in% c(0,1) & date_time > as.POSIXct("2024-06-25"))) 

site_HOBO_all %>%
  filter(date_time > "2024-07-15" & date_time < "2024-08-15") %>%
  filter(depth_from_top %in% c("0",6)) %>%
  ggplot(aes(x=date_time, y=temperature_C, color=factor(depth_from_top)))+
  geom_point(alpha=0.2)

#Convert to wide format
site_WY_HOBO_temperature <- site_HOBO_all %>%
  filter(!flag == "above water") %>%
  ungroup() %>%
  select(date_time, temperature_C, depth_from_top) %>%
  group_by(depth_from_top) %>%
  mutate(date_time_new = floor_date(date_time, "5 mins")) %>%
  select(-date_time) %>%
  pivot_wider(names_from = c(depth_from_top),
              values_from = temperature_C,
              names_prefix = "wtr_",
              values_fn = mean)

# ALL THE TEMPERTATURES!
site_WY <- full_join(WY_miniDOT_temperature,site_WY_HOBO_temperature)

# Convert to long and look at them together 
site_WY %>%
  pivot_longer(-date_time_new) %>%
  ggplot(aes(x=date_time_new, y=value, color=name))+
  geom_point()


site_WY <- site_WY %>%
  rename(date_time=date_time_new) %>%
  relocate(wtr_1, .before = wtr_6.5) %>%
  relocate(wtr_2.5, .after = wtr_2) %>%
  relocate(wtr_6.5, .after = everything()) %>%
  relocate(wtr_3, .after = wtr_2.5) %>%
  relocate(wtr_5, .after = wtr_4) %>%
  relocate(wtr_0, .before = wtr_1) %>%
  rename(wtr_0.25=wtr_0) %>%
  rename(datetime=date_time) %>%
  mutate(local_TZ="America/Denver",
         daylight_savings="yes")
write.table(sky_WY2023, "data_export/mtn_sensor_network/Oleksy_SouthernRockies_WY2024_SkyPond_temperature.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)
  relocate(wtr1, .before = wtr6.5) %>%
  relocate(wtr2.5, .after = wtr2) %>%
  relocate(wtr6.5, .after = everything()) %>%
  relocate(wtr3, .after = wtr2.5) %>%
  relocate(wtr5, .after = wtr4) %>%
  relocate(wtr0, .before = wtr1) %>%
  rename(wtr0.25=wtr0) %>%
  rename(datetime=date_time)

# save output file
write_csv(site_WY, "data_export/mtn_sensor_network/Oleksy_SouthernRockies_WY2024_SkyPond_temperature.txt")

sky_WY2023_daily <- sky_WY2023 %>%
  pivot_longer(wtr_0.25:wtr_6.5) %>%
  mutate(date=date(datetime)) %>%
  group_by(date, name) %>%
  summarize(value=mean(value, na.rm=TRUE)) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  rename(datetime=date)
write.table(sky_WY2023_daily, "data_export/mtn_sensor_network/Oleksy_SouthernRockies_WY2024_SkyPond_temperature_dailysummary.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)

sky_WY2023_graph <- load.ts("data_export/mtn_sensor_network/Oleksy_SouthernRockies_WY2024_SkyPond_temperature_dailysummary.txt")

sky_WY2023_graph_winter <- sky_WY2023_graph %>%
  filter(datetime < "2024-06-15")

sky_WY2023_graph_summer <- sky_WY2023_graph %>%
  filter(datetime > "2024-07-01") %>%
  select(-wtr_2.5)

library(rLakeAnalyzer)
wtr.heat.map(sky_WY2023_graph_winter)
wtr.heat.map(sky_WY2023_graph_summer)
wtr.plot.temp(sky_WY2023_graph_summer)
