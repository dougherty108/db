source("functions/00_libraries.R")
source("functions/00_helper_functions.R")
source("functions/01_ysi_profile.R")


# Inspect the profiles, summarize the data, and export 

# 00 Set Up R Environment ----
# Write plotting function 

Round_Plot_YSI_FUNC <- function(ysi_profile, round_to_nearest ){
  ysi_profile %>%
    mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest ) %>% #round to the nearest 0.5
    group_by(depth_m, parameter, lake) %>%
    mutate(value = median(value, na.rm=TRUE)) %>%
    mutate(month=month(date_time)) %>%
    filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
    ggplot(aes(x=value, y=depth_m, color=parameter))+
    geom_point()+
    scale_y_reverse()+
    facet_wrap(parameter~., scales="free_x", nrow = 2)+
    labs(title=paste(unique(ysi_profile$lake),unique(ysi_profile$date)))+
    theme(legend.position="none")
}

# 01 Load and format data ----

# Load in Data 

# LFM 
LFM_dir <- here("data/Sensors/YSI Pro DSS/LFM/raw")
LFMfiles <- dir_ls(LFM_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
LFMfiles <- LFMfiles[str_detect(LFMfiles, "Zmax")]  # Only look at the "Zmax" files
length(LFMfiles) #check how many files you have 

# UFM 
UFM_dir <- here("data/Sensors/YSI Pro DSS/UFM/raw")
UFMfiles <- dir_ls(UFM_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
UFMfiles <- UFMfiles[str_detect(UFMfiles, "Zmax")]  # Only look at the "Zmax" files
length(UFMfiles) #check how many files you have 

# TUC 
TUC_dir <- here("data/Sensors/YSI Pro DSS/TUC/raw")
TUCfiles <- dir_ls(TUC_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
TUCfiles <- TUCfiles[str_detect(TUCfiles, "Zmax")]  # Only look at the "Zmax" files
length(TUCfiles) #check how many files you have 


# Process and Clean YSI profiles 

# LFM
LFM_1 <- process_ysi(LFMfiles[1])
LFM_2 <- process_ysi(LFMfiles[2])
LFM_3 <- process_ysi(LFMfiles[3])

# UFM
UFM_1 <- process_ysi(UFMfiles[1])
UFM_2 <- process_ysi(UFMfiles[2])
UFM_3 <- process_ysi(UFMfiles[3])

# TUC
TUC_1 <- process_ysi(TUCfiles[1])
TUC_2 <- process_ysi(TUCfiles[2])
TUC_3 <- process_ysi(TUCfiles[3])



# 02 Visualize Profiles  ---------------------------------------------

# LFM 
Round_Plot_YSI_FUNC(LFM_1, 0.25) 
Round_Plot_YSI_FUNC(LFM_2, 0.25) 
Round_Plot_YSI_FUNC(LFM_3, 0.25)

# UFM 
Round_Plot_YSI_FUNC(UFM_1, 0.25) # just a point measurement
Round_Plot_YSI_FUNC(UFM_2, 0.25) 
Round_Plot_YSI_FUNC(UFM_3, 0.25)

# TUC 
Round_Plot_YSI_FUNC(TUC_1, 0.25) 
Round_Plot_YSI_FUNC(TUC_2, 0.25) 
Round_Plot_YSI_FUNC(TUC_3, 0.25)



# 03 Export csv with rounded depths  ---------------------------------------------

#LFM 
LFM_20240718 <- LFM_1 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write.csv(LFM_20240718, "data/Sensors/YSI Pro DSS/LFM/export/LFM_20240718_profile.csv")

LFM_20240906 <- LFM_2 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(LFM_20240906, "data/Sensors/YSI Pro DSS/LFM/export/LFM_20240906_profile.csv")

LFM_20250716 <- LFM_3 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(LFM_20250716, "data/Sensors/YSI Pro DSS/LFM/export/LFM_20250716_profile.csv")


# UFM - just a point measurement
# UFM_20240718 <- UFM_1 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
#   group_by(lake, date, depth_m, parameter) %>%
#   summarize(value = median(value, na.rm=TRUE)) 
# write_csv(UFM_20240718, here("data/Sensors/YSI Pro DSS/UFM/export/UFM_20240718_profile.csv"))
#

UFM_20240907 <- UFM_2 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(UFM_20240907, "data/Sensors/YSI Pro DSS/UFM/export/UFM_20240907_profile.csv")

UFM_20250716 <- UFM_3 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(UFM_20250716, "data/Sensors/YSI Pro DSS/UFM/export/UFM_20250716_profile.csv")


#TUC 
TUC_20240716 <- TUC_1 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(TUC_20240716, "data/Sensors/YSI Pro DSS/TUC/export/TUC_20240716_profile.csv")

TUC_20240906 <- TUC_2 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(TUC_20240906, "data/Sensors/YSI Pro DSS/TUC/export/TUC_20240906_profile.csv")

TUC_20250716 <- TUC_3 %>%
  mutate(depth_m=round(depth_m/0.25)*0.25) %>% 
  group_by(lake, date, depth_m, parameter) %>%
  summarize(value = median(value, na.rm=TRUE)) 
write_csv(TUC_20250716, "data/Sensors/YSI Pro DSS/TUC/export/TUC_20250716_profile.csv")

