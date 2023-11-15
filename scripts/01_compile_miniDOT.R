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

#this section has not been updated as of 11/8 -AGK
#flag outliers
#calculate z-scores, flag values >3 sd from mean (?)
#convert to long format - column with temp, do_obs, do_sat
#split date_time into month and year
sky_flag <- sky_minidot %>% 
  mutate(year = year(date_time), 
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

loch_flag <- loch_minidot %>%
  mutate(year = year(date_time), 
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

fern_flag <- fern_minidot %>%
  mutate(year = year(date_time),
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

#plot data####
#do_obs####
sky_do <- ggplot(data = sky_minidot %>%
                   filter(do_obs < 100), aes(x = date_time, y = do_obs, color = depth))+
  geom_point(aes(color = depth))
sky_do

loch_do <- ggplot(data = loch_minidot, aes(x = date_time, y = do_obs, color = depth))+
  geom_point(aes(color = depth))
loch_do

fern_do <- ggplot(data = fern_minidot, aes(x = date_time, y = do_obs, color = depth)) +
  geom_point(aes(color = depth))
fern_do
#wondering where the rest of the surface data is?

#do_sat####
#this needs to be calculated prior to be being plotted

#temp####
sky_temp <- ggplot(data = sky_minidot, aes(x = date_time, y = temp, color = depth))+
  geom_point(aes(color = depth))
sky_temp

loch_temp <- ggplot(data = loch_minidot, aes(x = date_time, y = temp, color = depth))+
  geom_point(aes(color = depth))
loch_temp

fern_temp <- ggplot(data = fern_minidot, aes(x = date_time, y = temp, color = depth)) +
  geom_point(aes(color = depth))
fern_temp

#plot z-scores for do_sat, do_obs, and temp for sky pond and loch
sky_do_scaled <- ggplot(data = sky_flag %>%
                      filter(name == "do_obs"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
sky_do_scaled

# sky_dosat_scaled <- ggplot(data = sky_flag %>%
#                       filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
#                       geom_point(aes(shape = flag))+
#                       facet_wrap(~depth, scales = "free_y")
sky_dosat_scaled
#4x as a cutoff? should be grouped seasonally first though
sky_temp_scaled <- ggplot(data = sky_flag %>%
                      filter(name == "temp"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
sky_temp_scaled

loch_do_scaled <- ggplot(data = loch_flag %>%
                      filter(name == "do_obs"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
loch_do_scaled

# loch_dosat_scaled <- ggplot(data = loch_flag %>%
#                       filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
#                       geom_point(aes(shape = flag))+
#                       facet_wrap(~depth, scales = "free_y")
loch_dosat_scaled

loch_temp_scaled <- ggplot(data = loch_flag %>% 
                      filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
loch_temp_scaled


fern_do_scaled <- ggplot(data = fern_flag %>%
                      filter(name == "do_obs"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
fern_do_scaled


fern_temp_scaled <- ggplot(data = fern_flag %>%
                      filter(name == "temp"), aes(x = date_time, y = value_scale, color = flag)) +
                      geom_point(aes(shape = flag)) + 
                      facet_wrap(~ depth, scales = "free_y")

fern_temp_scaled


#xbar to visualize outliers - this isn't great but it's functional. AGK
xbar_sky <- ggplot(sky_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_sky


xbar_loch <- ggplot(loch_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_loch

xbar_fern <- ggplot(fern_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_fern







