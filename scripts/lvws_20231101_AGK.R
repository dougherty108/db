#LVWS miniDOT data (sky, loch, fern) 2018-2023
#version 11/1/23 AGK

#setup####
#install.packages("ggQC")
library(tidyverse)
library(ggQC)
# library(tibble)
# library(ggplot2)
library(plyr)
# library(dplyr)
library(dtplyr)
library(Rmisc)
library(ggpubr)
library(rstatix)
library(lubridate)

#this currently works with local files, will modify to work with onedrive/teams files
  #by end of day tomorrow -AGK
setwd("~/Desktop")

#read in raw file, select columns (date_time, temp, do, do_sat), rename columns
#add columns lake_id, local_tz, daylight_savings, depth

#build sky pond database####
#using bind_rows to build database while reading files in via read.table
#these files have already been concatenated, just pulling together separate year files. 
  #I manually delimited these in excel and removed the line of units. can change if needed -AGK
sky_minidot <- bind_rows(read.table("Sky_LS_20180625_20180912.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = GMT.07.00, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_20190806_20200714.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_20200721_20210624.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_20210706_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LH_20180924_20190731.txt", sep = "\t", header = TRUE, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature_C, do_obs = DissolvedOxygen_mgL, 
    do_sat = DissolvedOxygenSaturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20190806_20200715.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20200721_20210624.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20210928_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_Mid_20210928_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "3.5"))

#trimmed sky pond database####
#this was trimmed to fit the timeframe of another database, not due to data issues/outliers (per tim) -AGK
trimmed_sky_minidot <- bind_rows(read.table("Sky_LS_20180625_20180912.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = GMT.07.00, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_20190806_20200714.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_2020_2021.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LS_20210706_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Sky_LH_20180924_20190731.txt", sep = "\t", header = TRUE, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature_C, do_obs = DissolvedOxygen_mgL, 
    do_sat = DissolvedOxygenSaturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20190806_20200715.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20200721_20210624.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_LH_20210928_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6"), 
  read.table("Sky_Mid_20210928_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "3.5"))

#trimmed 2021 sky pond data####

#build loch database####
#bind_rows to build database while reading in files
  #see above comment about sky pond delimiting/file editing -AGK
loch_minidot <- bind_rows(read.table("Loch_LS_20180924_20191003.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Loch_LS_20190619_20200526.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Loch_LS_20200602_20210608.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Loch_LS_20210615_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5"), 
  read.table("Loch_LH_20200602_20210608.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4"), 
  read.table("Loch_LH_20210615_20220809.txt", sep = "\t", header = TRUE, skip = 7, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    rename(date_time = Mountain.Standard.Time, temp = Temperature, do_obs = Dissolved.Oxygen, 
    do_sat = Dissolved.Oxygen.Saturation) %>% 
    mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4"))

#build fern database####
#fern miniDOT files are not concatenated - code below does so manually
  #no do_sat - need to calculate
#read in separate fern folders (21_22 and 22_23 hypo and surface)
fernLH_21_22 <- fs::dir_ls("~/Desktop/2021_22_FernLH", regexp = "\\.txt$") %>%
  purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
  select(1, 3, 4) %>%
  dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
  mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "5") %>%
  mutate(new_date = as_datetime(`date_time`))
fernLS_21_22 <- fs::dir_ls("~/Desktop/2021_22_FernLS", regexp = "\\.txt$") %>%
  purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
  select(1, 3, 4) %>%
  dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
  mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
  mutate(new_date = as_datetime(`date_time`))
fernLH_22_23 <- fs::dir_ls("~/Desktop/2022_23_FernLH", regexp = "\\.txt$") %>%
  purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
  select(1, 3, 4) %>%
  dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
  mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "5") %>%
  mutate(new_date = as_datetime(`date_time`))
fernLS_22_23 <- fs::dir_ls("~/Desktop/2022_23_FernLS", regexp = "\\.txt$") %>%
  purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
  select(1, 3, 4) %>%
  dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
  mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
  mutate(new_date = as_datetime(`date_time`))

#use bind_rows to build database 
fern_minidot <- bind_rows(fernLH_21_22, fernLS_21_22, fernLH_22_23, fernLS_22_23)

#factors & change date formatting####
#depth as a factor for all data frames, date in ymd_hms
sky_minidot$depth <- as.factor(sky_minidot$depth)
sky_minidot <- sky_minidot %>%
  mutate(date_time = ymd_hms(`date_time`))
# trimmed_sky_minidot$depth <- as.factor(trimmed_sky_minidot$depth)
# trimmed_sky_minidot <- trimmed_sky_minidot %>%
  # mutate(date_time = ymd_hms(`date_time`))
loch_minidot$depth <- as.factor(loch_minidot$depth)
loch_minidot <- loch_minidot %>%
  mutate(date_time = ymd_hms(`date_time`))
fern_minidot$depth <- as.factor(fern_minidot$depth)
fern_minidot <- fern_minidot %>%
  mutate(new_date = ymd_hms(`new_date`))

#flag outliers
#calculate z-scores, flag values >3 sd from mean (?)
#convert to long format - column with temp, do_obs, do_sat
#split date_time into month and year
sky_flag <- sky_minidot %>% 
  mutate(year = year(date_time), 
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs, do_sat)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

# sky_flag_trim <- trimmed_sky_minidot %>% 
#   mutate(year = year(date_time), 
#          month = month(date_time)) %>%
#   pivot_longer(c(temp, do_obs, do_sat)) %>%
#   group_by(month, depth, year, name) %>%
#   mutate(value_scale = scale(value), 
#          flag = case_when(abs(value_scale) > 3 ~ "yes", 
#                           .default = "no"))

loch_flag <- loch_minidot %>%
  mutate(year = year(date_time), 
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs, do_sat)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

fern_flag <- fern_minidot %>%
  mutate(year = year(new_date),
         month = month(new_date)) %>%
  pivot_longer(c(temp, do_obs)) %>%
  group_by(month, depth, year, name) %>%
  mutate(value_scale = scale(value), 
         flag = case_when(abs(value_scale) > 3 ~ "yes", 
                          .default = "no"))

#plot data####
#plot raw data####
#do_obs####
#trimmed data 
# trim_sky_do <- ggplot(data = trimmed_sky_minidot, aes(x = date_time, y = do_obs, color = depth))+
#   geom_point(aes(color = depth))
# trim_sky_do
#raw data
sky_do <- ggplot(data = sky_minidot %>%
                   filter(do_obs < 100), aes(x = date_time, y = do_obs, color = depth))+
  geom_point(aes(color = depth))
sky_do

loch_do <- ggplot(data = loch_minidot, aes(x = date_time, y = do_obs, color = depth))+
  geom_point(aes(color = depth))
loch_do

fern_do <- ggplot(data = fern_minidot, aes(x = new_date, y = do_obs, color = depth)) +
  geom_point(aes(color = depth))
fern_do
#wondering where the rest of the surface data is?

#do_sat####
# trim_sky_sat <- ggplot(data = trimmed_sky_minidot, aes(x = date_time, y = do_sat, color = depth))+
#   geom_point(aes(color = depth))
# trim_sky_sat
#raw data
sky_sat <- ggplot(data = sky_minidot %>%
                   filter(do_sat < 200), aes(x = date_time, y = do_sat, color = depth))+
  geom_point(aes(color = depth))
sky_sat

loch_sat <- ggplot(data = loch_minidot, aes(x = date_time, y = do_sat, color = depth))+
  geom_point(aes(color = depth))
loch_sat

#need to calculate fern do_sat

#temp####
# trim_sky_temp <- ggplot(data = trimmed_sky_minidot, aes(x = date_time, y = temp, color = depth))+
#   geom_point(aes(color = depth))
# trim_sky_temp

sky_temp <- ggplot(data = sky_minidot, aes(x = date_time, y = temp, color = depth))+
  geom_point(aes(color = depth))
sky_temp

loch_temp <- ggplot(data = loch_minidot, aes(x = date_time, y = temp, color = depth))+
  geom_point(aes(color = depth))
loch_temp

fern_temp <- ggplot(data = fern_minidot, aes(x = new_date, y = temp, color = depth)) +
  geom_point(aes(color = depth))
fern_temp

#plot z-scores for do_sat, do_obs, and temp for sky pond and loch
sky_do_scaled <- ggplot(data = sky_flag %>%
                      filter(name == "do_obs"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
sky_do_scaled

sky_dosat_scaled <- ggplot(data = sky_flag %>%
                      filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
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

loch_dosat_scaled <- ggplot(data = loch_flag %>%
                      filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
loch_dosat_scaled

loch_temp_scaled <- ggplot(data = loch_flag %>% 
                      filter(name == "do_sat"), aes(x = date_time, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
loch_temp_scaled


fern_do_scaled <- ggplot(data = fern_flag %>%
                      filter(name == "do_obs"), aes(x = new_date, y = value_scale, color = flag))+
                      geom_point(aes(shape = flag))+
                      facet_wrap(~depth, scales = "free_y")
fern_do_scaled


fern_temp_scaled <- ggplot(data = fern_flag %>%
                      filter(name == "temp"), aes(x = new_date, y = value_scale, color = flag)) +
                      geom_point(aes(shape = flag)) + 
                      facet_wrap(~ depth, scales = "free_y")

fern_temp_scaled
#need to calculate do_sat

#finding outliers - z-score, look into other methods
  #z-score is best when the data follow a normal distribution - do they?

#xbar to visualize outliers - this isn't great but it's functional. AGK
xbar_sky <- ggplot(sky_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_sky

xbar_sky_trim <- ggplot(trimmed_sky_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_sky_trim

xbar_loch <- ggplot(loch_minidot, aes(x = date_time, y = do_obs, group = 1)) + 
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  stat_QC(method = "XmR") 
xbar_loch
