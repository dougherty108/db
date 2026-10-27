# Written by Bella Oleksy 2024-04-08
# SKY sleuthing 
# Run the HOBO and MINIDOT scripts and then this will all work


# data munging ------------------------------------------------------------


SKY2024 <- combined_data_clean %>%
  filter(lake_id=="SKY" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(temp < 20) 
  # filter(water_year %in% c('2024'))
head(SKY2024)

SKY2024minidot <- SKY2024 %>%
  ungroup() %>%
  select(date_time, temp, do_obs, lake_id, depth_from_top) %>%
  rename(temperature_C = temp) %>%
  mutate(data_type = "miniDOT",
         depth_from_top = as.numeric(depth_from_top))

SKY2024hobo <- all_HOBO %>%
  filter(lake_ID == "SKY") %>%
  select(date_time, temperature_C, depth_from_top) %>%
  mutate(data_type = "HOBO")

SKY2024bind <- bind_rows(SKY2024minidot, SKY2024hobo)

LOC2024 <- combined_data_clean %>%
  filter(lake_id=="LOC") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(temp < 20) 
  # filter(water_year %in% c('2024'))
names(LOC2024)

LOC2024minidot <- LOC2024 %>%
  ungroup() %>%
  select(date_time, temp, do_obs, lake_id, depth_from_top) %>%
  rename(temperature_C = temp) %>%
  mutate(data_type = "miniDOT",
         depth_from_top = as.numeric(depth_from_top))

LOC2024hobo <- all_HOBO %>%
  filter(lake_ID == "LOC") %>%
  select(date_time, temperature_C, depth_from_top, depth_from_bottom) %>%
  mutate(data_type = "HOBO")

LOC2024bind <- bind_rows(LOC2024minidot, LOC2024hobo)

# SKY Pond graphs ------------------------------------------------------------------



# Just looking at the HOBO sensors in SKY Pond first
SKY2024bind %>%
  filter(data_type =="HOBO") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  filter(!depth_from_top %in% c("0","1")) %>% #these are frozen
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(SKY2024bind$lake_id)))

# Just miniDOT - temperature
SKY2024bind %>%
  filter(data_type =="miniDOT") %>%
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
  labs(title=paste(unique(SKY2024bind$lake_id)))

# Just miniDOT - oxygen
SKY2024bind %>%
  filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  # filter(!depth_from_top %in% c("0.2","1.2")) %>% #these are frozen
  ggplot(aes(
    x = date_time,
    y = do_obs,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(SKY2024bind$lake_id)))


# Both HOBOs and miniDOTS together
SKY2024bind %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 100) %>%
  # filter(depth_from_top=="0.5") %>%
  filter(!depth_from_top %in% c("0","1")) %>% #these are frozen
  # filter(depth_from_top %in% c("0.5","2.2","3.2")) %>% #these are frozen
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(SKY2024bind$lake_id)))
  
# facet_wrap(.~depth_from_top)

#Look at just the 6 and 6.5m sensors
SKY2024bind %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 100) %>%
  # filter(depth_from_top=="0.5") %>%
  filter(depth_from_top %in% c("6","6.5")) %>% 
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point() +
  geom_line() +
  labs(title=paste(unique(SKY2024bind$lake_id)))
# This is the only thing that seems a bit sus now. Shouldn't the 6.5m miniDOT be reading warmer than the 6m HOBO?

# The LOC graphs ------------------------------------------------------------------


# The file name says the sensor placement is from the top, but that doesn't seem to be the case
# Based on the 
# Just looking at the HOBO sensors in The LOC
LOC2024bind %>%
  filter(data_type =="HOBO") %>%
  mutate(doy = yday(date_time)) %>%
  # filter(doy < 150) %>%
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
  labs(title=paste(unique(LOC2024bind$lake_id)))

#Just miniDOT
LOC2024bind %>%
  filter(data_type =="miniDOT") %>%
  filter(date_time > "2024-07-01") %>%
  mutate(doy = yday(date_time)) %>%
  # filter(doy < 150) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top)    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(LOC2024bind$lake_id)))


#Both
LOC2024bind %>%
  # filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(date_time > "2024-10-26" & date_time < "2025-05-20") %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point(alpha=0.1) +
  labs(title=paste(unique(LOC2024bind$lake_id)))

#Just look at the daily mean temp at each sensor depth x data_type combo
LOC2024bind %>%
  filter(date_time > "2024-10-26" & date_time < "2025-05-20") %>%
  mutate(doy = yday(date_time)) %>%
  group_by(doy, data_type, depth_from_top) %>%
  summarize(temperature_C = mean(temperature_C, na.rm=TRUE)) %>%
  filter(doy > 150 ) %>%
  ggplot(aes(
    x = doy,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point(alpha=0.5) +
  labs(title=paste(unique(LOC2024bind$lake_id)))


#Duplicate alleged 4m from top sensors
#Both
LOC2024bind %>%
  # filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  filter(depth_from_top %in% c("4")) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(data_type),
    shape = data_type
  )) +
  geom_point() +
  geom_line() +
  labs(title=paste(unique(LOC2024bind$lake_id)))

LOC2024bind %>%
  # filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  # filter(depth_from_top %in% c("0.5","2","3")) %>%
  mutate(depth_from_top = case_when(as.character(depth_from_top) == "0.5" ~ "1.5",
                                    TRUE ~ as.character(depth_from_top))) %>%
  ggplot(aes(
    x = date_time,
    y = temperature_C,
    color = factor(depth_from_top),
    shape = data_type
  )) +
  geom_point() +
  geom_line() +
  labs(title=paste(unique(LOC2024bind$lake_id)))

#How does DO data look?
LOC2024bind %>%
  filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy < 150) %>%
  ggplot(aes(
    x = date_time,
    y = do_obs,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point() +
  labs(title=paste(unique(LOC2024bind$lake_id)))

#How does DO data look along with temperature?
LOC2024bind %>%
  filter(data_type =="miniDOT") %>%
  mutate(doy = yday(date_time)) %>%
  pivot_longer(temperature_C:do_obs) %>%
  filter(date_time > "2024-05-01" & date_time < "2024-06-13") %>%
  ggplot(aes(
    x = date_time,
    y = value,
    color = factor(name),
    # shape = data_type
  )) +
  geom_point() +
  geom_hline(yintercept=4)+
  labs(title=paste(unique(LOC2024bind$lake_id))) +
  facet_wrap(~depth_from_top)



# Light from HOBOs --------------------------------------------------------

light2024hobo <- all_HOBO %>%
  filter(lake_ID %in% c("SKY","LOC")) %>%
  select(date_time, lake_ID, light_lux, depth_from_top) %>%
  mutate(data_type = "HOBO")

#LOC
light2024hobo %>%
  filter(lake_ID=="LOC") %>%
  mutate(doy = yday(date_time)) %>%
  filter(doy > 50 & doy < 75) %>%
  filter(light_lux > 10) %>%
  ggplot(aes(
    x = date_time,
    y = light_lux,
    color = factor(depth_from_top),
    # shape = data_type
  )) +
  geom_point(alpha=0.5) +
  geom_line()
  # facet_grid(depth_from_top~lake_ID, scales="free_y")


