source("scripts/00_libraries.R")
source("scripts/00_functions.R")


#Compile all San Juans profiles 
LFM_july <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240718.csv")
LFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/LOWER 4MILE/raw/LowerFourmile_Lake_20240906.csv")
UFM_sept <- process_ysi("Data/USFS San Juans/01_YSI/UPPER 4MILE/raw/UpperFourmile_Lake_20240907.csv")
TKY_july <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240716.csv")
TKY_sept <- process_ysi("Data/USFS San Juans/01_YSI/TKY CREEK/raw/TurkeyCreek_Lake_20240906.csv")

SJ_all <- bind_rows(LFM_july,
                    LFM_sept,
                    UFM_sept,
                    TKY_july,
                    TKY_sept)

SJ_all %>%
  mutate(month=month(date_time)) %>%
  filter(paramter=="temp") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)


#Can we round the depth values to smooth out the profiles?
SJ_all %>%
  mutate(depth=round(depth/0.1)*0.1) %>% #round to the nearest 0.1
  group_by(depth, paramter, lake) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(paramter=="do_mgL") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(lake~.)
#Maybe?

#Can we round the time values to smooth out the profiles?
SJ_all %>%
  mutate(rounded_timestamp = as.POSIXct(round(as.numeric(date_time) / 5) * 5, origin = "1970-01-01")) %>%
  #round to nearest 5 seconds
  group_by(rounded_timestamp, lake, paramter) %>%
  mutate(value = median(value, na.rm=TRUE)) %>%
  mutate(month=month(date_time)) %>%
  filter(paramter=="do_mgL") %>%
  ggplot(aes(x=value, y=depth, color=factor(month)))+
  geom_point()+
  facet_wrap(lake~.)
#Maybe?
