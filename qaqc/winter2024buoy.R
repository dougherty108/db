source("functions/07_EXO3.R")
source("functions/00_helper_functions.R")
source("functions/00_libraries.R")
source("functions/05_OdysseyPAR.R")

summer1 <- process_EXO("data/Sensors/YSI EXO3/LOC_EXO_Xm_BOT_20240815_20241017.csv") %>% mutate(time_period="summer1")
winter1 <- process_EXO("data/Sensors/YSI EXO3/LOC_EXO_2.3m_BOT_20241024_20250430.csv") %>% mutate(time_period="winter1")
winter2 <- process_EXO("data/Sensors/YSI EXO3/LOC_EXO_2m_BOT_20250313_20250523.csv") %>% mutate(time_period="winter2")


#  Question 1 - what do these columns for Chlorophyll/PE/PC mean  --------


winter1 %>%
  ggplot(aes(x=total_algae_PE, y=TAL_PE_RFU))+
  geom_point(alpha=0.1) +
  labs(x = "Chlorophyll RFU (PE)")+
  geom_smooth(method="lm", se=FALSE, linewidth=0.2, color="black")
#These are correlated with each other mostly

winter1 %>%
  ggplot(aes(x=total_algae_PC, y=TAL_PC_RFU))+
  geom_point(alpha=0.1)+
  labs(x = "Chlorophyll RFU (PC)")+
  geom_smooth(method="lm", se=FALSE, linewidth=0.2, color="black")
#These are correlated with each other mostly

winter1 %>%
  ggplot(aes(x=total_algae_PC, y=total_algae_PE))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE, linewidth=0.2, color="black")
#Not correlated with each other

winter1 %>%
  filter(total_algae_PC < 2) %>%
  ggplot(aes(x=total_algae_PC, y=total_algae_PE))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE, linewidth=0.2, color="black")

winter1 %>%
  # filter(date_time > "2025-01-01") %>%
  mutate(month = month(date_time)) %>%
  ggplot(aes(x=date_time, y=total_algae_PE+total_algae_PC))+
  geom_point()+
  facet_wrap(~month, scales="free")
#Does it make sense to sum to the two chlorophyll columns for total chlorophyll?

winter1 %>%
  filter(total_algae_PC < 5) %>%
  pivot_longer(c(total_algae_PC,total_algae_PE)) %>%
  ggplot(aes(x=date_time, y=value, color=name))+
  geom_point(alpha=0.1)


winter1 %>%
  # filter(date_time > "2025-01-01") %>%
  mutate(month = month(date_time)) %>%
  ggplot(aes(x=date_time, y=TAL_PE_RFU/total_algae_PE))+
  geom_point()

#  Question 2 - battery life over time?  --------
winter1 %>%
  ggplot(aes(x=date_time, y=battery_V))+
  geom_point(alpha=0.2)

winter2 %>%
  ggplot(aes(x=date_time, y=battery_V))+
  geom_point(alpha=0.2)

#  Question 3 - compare the same parameters across winter 1 and 2 where there is overlap --------

names(winter1)

winter1_long <- winter1 %>%
  select(date_time, time_period, total_algae_PC:cable_power_V) %>%
  pivot_longer(total_algae_PC:cable_power_V)

winter2_long <- winter2 %>%
  select(date_time, time_period, total_algae_PC:cable_power_V) %>%
  pivot_longer(total_algae_PC:cable_power_V)

winter_all_long <- bind_rows(winter1_long, winter2_long) %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") #get period of overlap only

#  > Temperature  --------
winter_all_long %>%
  filter(name=="temp_C") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="temp_C") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_temp_C, winter2_temp_C))+
  geom_point()

winter_all_long %>%
  filter(name=="temp_C") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_degC = winter1_temp_C- winter2_temp_C) %>%
  ggplot(aes(x=date_time, y =daily_diff_degC))+
  geom_point()
#Minimal but also variable. Weird...

#  > total_algae_PC  --------
winter_all_long %>%
  filter(name=="total_algae_PC") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point(alpha=0.2)

winter_all_long %>%
  filter(name=="total_algae_PC") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_total_algae_PC, winter2_total_algae_PC))+
  geom_point()

winter_all_long %>%
  filter(name=="total_algae_PC") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_RFU = winter1_total_algae_PC- winter2_total_algae_PC) %>%
  ggplot(aes(x=date_time, y =daily_diff_RFU))+
  geom_point()

#  > total_algae_PE  --------
winter_all_long %>%
  filter(name=="total_algae_PE") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="total_algae_PE") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_total_algae_PE, winter2_total_algae_PE))+
  geom_point()

winter_all_long %>%
  filter(name=="total_algae_PE") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_RFU = winter1_total_algae_PE- winter2_total_algae_PE) %>%
  ggplot(aes(x=date_time, y =daily_diff_RFU))+
  geom_point()

#  > do_percent  --------
winter_all_long %>%
  filter(name=="do_percent") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="do_percent") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_do_percent, winter2_do_percent))+
  geom_point()

winter_all_long %>%
  filter(name=="do_percent") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_perc = winter1_do_percent- winter2_do_percent) %>%
  ggplot(aes(x=date_time, y =daily_diff_perc))+
  geom_point()

#  > do_percent CALIBRATED  --------
winter_all_long %>%
  filter(name=="do_percent_calibrated") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="do_percent_calibrated") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_do_percent_calibrated, winter2_do_percent_calibrated))+
  geom_point()

winter_all_long %>%
  filter(name=="do_percent_calibrated") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_perc = winter1_do_percent_calibrated- winter2_do_percent_calibrated) %>%
  ggplot(aes(x=date_time, y =daily_diff_perc))+
  geom_point()


#  > pressure_psia  --------
winter_all_long %>%
  filter(name=="pressure_psia") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="pressure_psia") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_pressure_psia, winter2_pressure_psia))+
  geom_point()

winter_all_long %>%
  filter(name=="pressure_psia") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff = winter1_pressure_psia- winter2_pressure_psia) %>%
  ggplot(aes(x=date_time, y =daily_diff))+
  geom_point()


#  > cond_uScm  --------
winter_all_long %>%
  filter(name=="cond_uScm") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="cond_uScm") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_cond_uScm, winter2_cond_uScm))+
  geom_point()

winter_all_long %>%
  filter(name=="cond_uScm") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff = winter1_cond_uScm- winter2_cond_uScm) %>%
  ggplot(aes(x=date_time, y =daily_diff))+
  geom_point()


#  > depth_m  --------
winter_all_long %>%
  filter(name=="depth_m") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point()

winter_all_long %>%
  filter(name=="depth_m") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_depth_m, winter2_depth_m))+
  geom_point()

winter_all_long %>%
  filter(name=="depth_m") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_m = winter1_depth_m- winter2_depth_m) %>%
  ggplot(aes(x=date_time, y =daily_diff_m))+
  geom_point()

#  > vertical_position_m  --------
winter_all_long %>%
  filter(name=="vertical_position_m") %>%
  ggplot(aes(x=date_time, y=value, color=time_period))+
  geom_point() +
  labs(y="Vertical position, m")

winter_all_long %>%
  filter(name=="vertical_position_m") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  ggplot(aes(x=winter1_vertical_position_m, winter2_vertical_position_m))+
  geom_point()

winter_all_long %>%
  filter(name=="vertical_position_m") %>%
  pivot_wider(names_from = c(time_period, name), values_from=value) %>%
  mutate(daily_diff_m = winter1_vertical_position_m- winter2_vertical_position_m) %>%
  ggplot(aes(x=date_time, y =daily_diff_m))+
  geom_point()




#  Question 4 - What is the actual depth of the EXO3s? compare temperature with miniDOTs... --------

# Compile miniDOT data
# source("functions/03_miniDOT.R")

# Just grab the late 2024 to early 2025 data
LOC_miniDOT2425 <- combined_data_clean %>%
  filter(date_time > "2024-11-01") %>%
  filter(lake_id == "LOC") %>%
  mutate(sensor = "miniDOT") %>%
  ungroup() %>%
  select(date_time, temp, do_obs, depth_from_bottom, sensor) %>%
  rename(temp_C=temp,
         do_mgL=do_obs) %>%
  pivot_longer(temp_C:do_mgL) %>%
  mutate(sensor = str_c(depth_from_bottom, sensor, sep = "m_")) %>%
  select(-depth_from_bottom)
head(LOC_miniDOT2425)

LOC_EXO2425 <- winter_all_long %>%
  filter(name %in% c("temp_C","do_mgL")) %>%
  rename(sensor = time_period) %>%
  mutate(sensor = case_when(sensor == "winter1" ~ "2.3m_EXO",
                            sensor == "winter2" ~ "unknown_EXO"))
head(LOC_EXO2425)

#Bind them together for graphing
all_2425 <- bind_rows(LOC_miniDOT2425,
                      LOC_EXO2425)

#Temperature - Get the time period of overlap all 5 sensors 
all_2425 %>%
  filter(name == "temp_C") %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") %>% #get period of overlap only
  ggplot() +
  geom_point(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(!sensor %in% c("0.5m_miniDOT")) %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") %>% #get period of overlap only
  ggplot() +
  geom_point(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(!sensor %in% c("0.5m_miniDOT")) %>%
  mutate(sensor = factor(sensor, levels = c("1.25m_miniDOT",
                                            "2m_miniDOT",
                                            "2.3m_EXO",
                                            "unknown_EXO"))) %>%
  filter(date_time > "2025-04-01" & date_time < "2025-04-15") %>% #get period of overlap only
  ggplot() +
  geom_line(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(sensor %in% c("2.3m_EXO","unknown_EXO")) %>%
  filter(date_time > "2025-04-01" & date_time < "2025-04-15") %>% #get period of overlap only
  ggplot() +
  geom_line(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(sensor %in% c("2m_miniDOT","unknown_EXO")) %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-01") %>% #get period of overlap only
  ggplot() +
  geom_line(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))


# How different are they from each other?
all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(sensor %in% c("2.3m_EXO","unknown_EXO")) %>%
  filter(date_time > "2025-04-01" & date_time < "2025-04-15") %>% #get period of overlap only
  mutate(date_time = round_date(date_time, unit = "30 minutes")) %>%
  pivot_wider(names_from = c(sensor,name),
              values_from= value) %>%
  mutate(doy = yday(date_time)) %>%
  ggplot(aes(x=`2.3m_EXO_temp_C`, `unknown_EXO_temp_C`, color=doy)) +
  geom_point()+
  geom_smooth(method="lm",se=FALSE) +
  geom_abline(intercept=0, slope=1)


# The temperature on winter2 EXO and 2m_miniDOT are pretty similar compare those two:
all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(sensor %in% c("2m_miniDOT","unknown_EXO")) %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") %>% #get period of overlap only
  mutate(date_time = round_date(date_time, unit = "30 minutes")) %>%
  pivot_wider(names_from = c(sensor,name),
              values_from= value) %>%
  mutate(doy = yday(date_time)) %>%
  ggplot(aes(x=`2m_miniDOT_temp_C`, `unknown_EXO_temp_C`, color=doy)) +
  geom_point()+
  geom_smooth(method="lm",se=FALSE) +
  geom_abline(intercept=0, slope=1)


# Do we see some weird temperature swings in the HOBO temperature data at some of the deeper depths? 
all_2425 %>%
  filter(name == "temp_C") %>% 
  filter(sensor %in% c("unknown_EXO")) %>%
  filter(date_time > "2025-04-01" & date_time < "2025-04-15") %>% #get period of overlap only
  ggplot() +
  geom_line(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))


# Oxygen - Get the time period of overlap all 5 sensors
all_2425 %>%
  filter(name == "do_mgL") %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") %>% #get period of overlap only
  ggplot() +
  geom_point(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_2425 %>%
  filter(name == "do_mgL") %>% 
  filter(!sensor %in% c("0.5m_miniDOT")) %>%
  filter(date_time > "2025-03-22" & date_time < "2025-04-30") %>% #get period of overlap only
  ggplot() +
  geom_point(aes(x = date_time, y = value, color = sensor), alpha = 0.5, show.legend = NA) +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))


#DECISIONS MADE AFTER QC:
# The "winter 1" deployment was corrected to 2.3m BOT
# The "winter 2" deployment was corrected to 2m BOT
# These are now hard-coded into the file names


# Question 5 - Odyssey PAR
# How do the data look?

LOC2m <- process_par("data/Sensors/Odyssey PAR/LOC/LOC_2m_BOT_20241025_20250522_Serial16694.CSV")
LOC1.5m <- process_par("data/Sensors/Odyssey PAR/LOC/LOC_1.5m_BOT_20241025_20250522_Serial16684.CSV")
LOC1m <- process_par("data/Sensors/Odyssey PAR/LOC/LOC_1m_BOT_20241025_20250522_Serial16691.CSV")

head(LOC2m)

all_PAR <- bind_rows(LOC2m,
                     LOC1.5m,
                     LOC1m)

all_PAR %>%
  mutate(month = month(date_time)) %>%
  filter(date_time > "2024-11-01" & date_time < "2025-05-20") %>%
  ggplot(aes(x=date_time, y=calibrated_value, color=factor(depth_from_top))) +
  geom_point(alpha=0.1, show.legend = NA) +
  facet_wrap(~month,scales="free") +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

all_PAR %>%
  mutate(month = month(date_time)) %>%
  filter(date_time > "2025-03-15" & date_time < "2025-04-20") %>%
  ggplot(aes(x=date_time, y=calibrated_value, color=factor(depth_from_top))) +
  geom_point(alpha=0.1, show.legend = NA) +
  # facet_wrap(~month,scales="free") +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

# Look at maximum daily values?
all_PAR %>%
  mutate(month = month(date_time),
         date = date(date_time)) %>%
  filter(date_time > "2024-11-01" & date_time < "2025-05-20") %>%
  group_by(date, depth_from_top) %>%
  summarize(max = max(calibrated_value)) %>%
  ggplot(aes(x=date, y=max, color=factor(depth_from_top))) +
  geom_point(alpha=0.2, show.legend = NA) +
  # facet_wrap(~month,scales="free") +
  guides(alpha = "none", color = guide_legend(override.aes = list(alpha = 1)))

# Look at maximum daily values?
look <- all_PAR %>%
  mutate(month = month(date_time),
         date = date(date_time)) %>%
  filter(date_time > "2024-11-01" & date_time < "2025-05-20") %>%
  group_by(date, depth_from_top) %>%
  summarize(max = max(calibrated_value)) %>%
  pivot_wider(names_from = depth_from_top,
              values_from = max) %>%
  mutate(diff_4_to_3.5 = `4` - `3.5`,
         diff_4_to_3 = `4` - `3`,
         diff_3.5_to_3 = `3.5` - `3`) 
  
