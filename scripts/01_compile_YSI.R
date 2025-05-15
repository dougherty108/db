source("scripts/00_libraries.R")
source("scripts/00_functions.R")


#Compile all San Juans profiles 
LFM_july <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240718.csv")
LFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240906.csv")
UFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/UPPER 4MILE/raw/UpperFourmile_Lake_20240907.csv")
TKY_july <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240716.csv")
TKY_sept <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240906.csv")
LOC_march <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250225.csv")
GL4_apr <- process_ysi("Data/On Thin Ice/01_YSI/GL4/raw/GL4_20250422.csv")

SJ_all <- bind_rows(LFM_july,
                    LFM_sept,
                    UFM_sept,
                    TKY_july,
                    TKY_sept)

SJ_all %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)


#Can we round the depth values to smooth out the profiles?
SJ_all %>%
  mutate(depth=round(depth/0.25)*0.25) %>% #round to the nearest 0.25
  group_by(depth, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="do_mgL") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)
#Maybe?

#Can we round the time values to smooth out the profiles?
SJ_all %>%
  mutate(rounded_timestamp = as.POSIXct(round(as.numeric(date_time) / 5) * 5, origin = "1970-01-01")) %>%
  #round to nearest 5 seconds
  group_by(rounded_timestamp, lake, parameter) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="do_mgL") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  facet_wrap(lake~.)
#Maybe?

# Do the more recent profiles look better?
LOC_march  %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)
# Yikes.

#Round to smooth out profiles?
LOC_march %>%
  mutate(depth=round(depth/0.25)*0.25) %>% #round to the nearest 0.1
  group_by(depth, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)
# Maybe the inverse strat is just easy to miss?

# Do the more recent profiles look better?
GL4_apr  %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)
# A little better than The Loch

#Round to smooth out profiles?
GL4_apr %>%
  mutate(depth=round(depth/0.5)*0.5) %>% #round to the nearest 0.5
  group_by(depth, parameter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(parameter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)+
  labs(x="Temp (deg C)")
# This looks very reasonable.

