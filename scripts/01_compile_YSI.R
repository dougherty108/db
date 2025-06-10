source("functions/00_libraries.R")
source("functions/00_helper_functions.R")
source("functions/01_ysi_profile.R")

# Profiles for Green Lake 4 (GL4) ---------------------------------------------

# Inspect the profiles, summarize the data, and export into "GL4 > export" folder

GL4_dir <- here("data/Sensors/YSI Pro DSS/GL4/raw")

# Get all text files in the main directory and its subdirectories
GL4files <- dir_ls(GL4_dir, regexp = "\\.csv$", recurse = TRUE)

# Only look at the "Zmax" files
GL4files <- GL4files[str_detect(GL4files, "Zmax")]
length(GL4files)


# ... 2024-06-27 ----------------------------------------------------------


GL4_1 <- process_ysi(GL4files[1])
head(GL4_1)

GL4_1 %>%
  mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(GL4_1$lake),unique(GL4_1$date)))

#Export a CSV with rounded depths
GL4_20240627 <- GL4_1 %>%
  mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(GL4_20240627, here("data/Sensors/YSI Pro DSS/GL4/export/GL4_20240627_profile.csv"))

# ... 2024-07-23 ----------------------------------------------------------

GL4_2 <- process_ysi(GL4files[2])
head(GL4_2)

GL4_2 %>%
  mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(GL4_2$lake),unique(GL4_2$date)))

#Export a CSV with rounded depths

# ... 2024-08-22 ----------------------------------------------------------

GL4_3 <- process_ysi(GL4files[3])
head(GL4_3)

GL4_3 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(GL4_3$lake),unique(GL4_3$date)))

#Export a CSV with rounded depths

# ... 2024-09-24 ----------------------------------------------------------

GL4_4 <- process_ysi(GL4files[4])
head(GL4_4)

GL4_4 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(GL4_4$lake),unique(GL4_4$date)))

#Export a CSV with rounded depths

# Profiles for The Loch (LOC) ---------------------------------------------

# Inspect the profiles, summarize the data, and export inot "LOC > export" folder

LOC_dir <- here("data/Sensors/YSI Pro DSS/LOC/raw")

# Get all text files in the main directory and its subdirectories
LOCfiles <- dir_ls(LOC_dir, regexp = "\\.csv$", recurse = TRUE)

# Only look at the "Zmax" files
LOCfiles <- LOCfiles[str_detect(LOCfiles, "Zmax")]
length(LOCfiles)

# ... 2024-03-05 ----------------------------------------------------------

LOC1 <- process_ysi(LOCfiles[1])
head(LOC1)

LOC1 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(LOC1$lake),unique(LOC1$date)))


# ... 2024-04-23 ----------------------------------------------------------

LOC2 <- process_ysi(LOCfiles[2])
head(LOC2)

LOC2 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(LOC2$lake),unique(LOC2$date)))


# ... 2024-05-13 ----------------------------------------------------------

LOC3 <- process_ysi(LOCfiles[3])
head(LOC3)

LOC3 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(LOC3$lake),unique(LOC3$date)))


# ... For dates with multiple holes, pick the best looking one and export that ----------------------------------------------------------
LOC14 <- process_ysi(LOCfiles[14])
head(LOC14)

LOC14 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(LOC14$lake),unique(LOC14$date)))

LOC16 <- process_ysi(LOCfiles[16])
head(LOC16)

LOC16 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
  group_by(depth_m, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
  ggplot(aes(x=value, y=depth_m, color=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(parameter~., scales="free_x", nrow = 2)+
  labs(title=paste(unique(LOC16$lake),unique(LOC16$date)))


# OTHER LAKES -------------------------------------------------------------


#Compile all San Juans profiles 
# LFM_july <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240718.csv")
# LFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240906.csv")
# UFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/UPPER 4MILE/raw/UpperFourmile_Lake_20240907.csv")
# TKY_july <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240716.csv")
# TKY_sept <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240906.csv")
# LOC_march <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250225.csv")
# GL4_apr <- process_ysi("Data/On Thin Ice/01_YSI/GL4/raw/GL4_20250422.csv")
# 
# SJ_all <- bind_rows(LFM_july,
#                     LFM_sept,
#                     UFM_sept,
#                     TKY_july,
#                     TKY_sept)
# 
# SJ_all %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# 
# 
# #Can we round the depth_m values to smooth out the profiles?
# SJ_all %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.25
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="do_mgL") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# #Maybe?
# 
# #Can we round the time values to smooth out the profiles?
# SJ_all %>%
#   mutate(rounded_timestamp = as.POSIXct(round(as.numeric(date_time) / 5) * 5, origin = "1970-01-01")) %>%
#   #round to nearest 5 seconds
#   group_by(rounded_timestamp, lake, parameter) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="do_mgL") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   facet_wrap(lake~.)
# #Maybe?
# 
# # Do the more recent profiles look better?
# LOC_march  %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # Yikes.
# 
# #Round to smooth out profiles?
# LOC_march %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.1
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # Maybe the inverse strat is just easy to miss?
# 
# # Do the more recent profiles look better?
# GL4_apr  %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)
# # A little better than The Loch
# 
# #Round to smooth out profiles?
# GL4_apr %>%
#   mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(parameter=="temp") %>%
#   ggplot(aes(x=value, y=depth_m, color=factor(month)))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(lake~.)+
#   labs(x="Temp (deg C)")
# # This looks very reasonable.
# 
