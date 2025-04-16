# Written by Bella Oleksy 2024-04-08
# Sky sleuthing 
# Run the HOBO and MINIDOT scripts and then this will all work


# data munging ------------------------------------------------------------


sky2024 <- combined_data_clean %>%
  filter(lake_id=="sky" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(temp < 20) %>%
  filter(water_year %in% c('2024'))
names(sky2024)

sky2024minidot <- sky2024 %>%
  select(date_time, temp, do_obs, lake_id, depth) %>%
  rename(depth_from_top = depth,
         temperature_C = temp) %>%
  mutate(data_type = "miniDOT",
         depth_from_top = as.numeric(depth_from_top))

sky2024hobo <- all_HOBO %>%
  filter(lake_ID == "SKY") %>%
  select(date_time, temperature_C, depth_from_top) %>%
  mutate(data_type = "HOBO")

sky2024bind <- bind_rows(sky2024minidot, sky2024hobo)

loch2024 <- combined_data_clean %>%
  filter(lake_id=="loch" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(temp < 20) %>%
  filter(water_year %in% c('2024'))
names(loch2024)

loch2024minidot <- loch2024 %>%
  select(date_time, temp, do_obs, lake_id, depth) %>%
  rename(depth_from_top = depth,
         temperature_C = temp) %>%
  mutate(data_type = "miniDOT",
         depth_from_top = as.numeric(depth_from_top))

loch2024hobo <- all_HOBO %>%
  filter(lake_ID == "LOC") %>%
  select(date_time, temperature_C, depth_from_top, depth_from_bottom) %>%
  mutate(data_type = "HOBO")

loch2024bind <- bind_rows(loch2024minidot, loch2024hobo)

# Sky Pond graphs ------------------------------------------------------------------



# Just looking at the HOBO sensors in Sky Pond first
sky2024bind %>%
  filter(data_type =="HOBO") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  # filter(!depth_from_top %in% c("0.2","1.2")) %>% #these are frozen
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(sky2024bind$lake_id)))


# Both HOBOs and miniDOTS together
sky2024bind %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 100) %>%
  # filter(!depth_from_top %in% c("0.2","1.2")) %>% #these are frozen
  filter(depth_from_top %in% c("0.5","2.2","3.2")) %>% #these are frozen
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(sky2024bind$lake_id)))

# This top most HOBO sensor (2.2m from top) doesn't match with the 0.5m miniDOT.
# Where is the miniDOT located, truly? The depth_from_top column for the HOBOs was 
# calculated from the bottom of the lake (subtracting from max depth of 7.2)
# because the 2.2m sensor is the coldest, my guess is that the miniDOT is actually
# quite a bit deeper than 0.5 m below the ice? More like 1.5m? 

# This is VERY COARSE (dont do this lol) but if you relabel the miniDOT depth
# to ~2.5 the sequence of sensors with temperatures makes a bit more sense
sky2024bind %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 100) %>%
  filter(!depth_from_top %in% c("0.2","1.2")) %>% #these are frozen
  mutate(depth_from_top = case_when(depth_from_top == 0.5 ~ 2.5,
                                    TRUE ~ depth_from_top)) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(sky2024bind$lake_id)))




# The Loch graphs ------------------------------------------------------------------


# The file name says the sensor placement is from the top, but that doesn't seem to be the case
# Based on the 
# Just looking at the HOBO sensors in The Loch
loch2024bind %>%
  filter(data_type =="HOBO") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  # filter(!depth_from_top %in% c("0.2","1.2")) %>% #these are frozen
  # mutate(depth_from_top = case_when(depth_from_top == 0.5 ~ 2.5,
  #                                   TRUE ~ depth_from_top)) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(loch2024bind$lake_id)))

#Just miniDOT
loch2024bind %>%
  filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(loch2024bind$lake_id)))


# Light from HOBOs --------------------------------------------------------

light2024hobo <- all_HOBO %>%
  filter(lake_ID %in% c("SKY","LOC")) %>%
  select(date_time, lake_ID, light_lux, depth_from_top) %>%
  mutate(data_type = "HOBO")

#Loch
light2024hobo %>%
  filter(lake_ID=="LOC") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  ggplot(aes(
    x = date_time,
    y = light_lux,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point(alpha=0.5) +
  facet_grid(depth_from_top~lake_ID, scales="free_y")


