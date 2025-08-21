source("functions/00_libraries.R")
source("functions/00_helper_functions.R")
source("functions/01_ysi_profile.R")






# Inspect the profiles, summarize the data, and export into "GL4 > export" folder

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
        labs(title=paste(unique(ysi_profile$lake),unique(ysi_profile$date)))
    }
    
    # Code to #Export a CSV with rounded depths
    GL4_20240627 <- GL4_1 %>%
      mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
      group_by(lake, date, depth_m, parameter) %>%
      summarize(value = median(value, na.rm=TRUE)) 
    write_csv(GL4_20240627, here("data/Sensors/YSI Pro DSS/GL4/export/GL4_20240627_profile.csv"))

# 01 Load and format data ----

  # Load in Data 
    
    # GL4 
    GL4_dir <- here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/raw/GL4")
    GL4files <- dir_ls(GL4_dir, regexp = "\\.csv$", recurse = TRUE)     # Get all text files in the main directory and its subdirectories
    # KAG 20250815 -- I downloaded the YSI profiles to my machine for easy access locally. For days with multiple YSI profiles from different ice holes I took only the deepest profile 
    GL4files <- GL4files[str_detect(GL4files, "Zmax")]  # Only look at the "Zmax" files
    length(GL4files) #check how many files you have 
    
    # LOC 
    LOC_dir <- here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/raw/LOC")
    LOCfiles <- dir_ls(LOC_dir, regexp = "\\.csv$", recurse = TRUE) # Get all text files in the main directory and its subdirectories
    LOCfiles <- LOCfiles[str_detect(LOCfiles, "Zmax")]     # Only look at the "Zmax" files
    length(LOCfiles) #check how many files you have 
    
    
  # Process and Clean YSI profiles 
    
    # GL4
    GL4_1 <- process_ysi(GL4files[1])
    GL4_2 <- process_ysi(GL4files[2])
    GL4_3 <- process_ysi(GL4files[3])
    GL4_4 <- process_ysi(GL4files[4])
    GL4_5 <- process_ysi(GL4files[5])
    GL4_6 <- process_ysi(GL4files[6])
    GL4_7 <- process_ysi(GL4files[7])
    GL4_8 <- process_ysi(GL4files[8])
    GL4_9 <- process_ysi(GL4files[9])
    GL4_10 <- process_ysi(GL4files[10])
    GL4_11 <- process_ysi(GL4files[11])
    GL4_12 <- process_ysi(GL4files[12])
    GL4_13 <- process_ysi(GL4files[13])
    
    # LOC 
    LOC_1 <- process_ysi(LOCfiles[1])
    LOC_2 <- process_ysi(LOCfiles[2])
    LOC_3 <- process_ysi(LOCfiles[3])
    LOC_4 <- process_ysi(LOCfiles[4])
    LOC_5 <- process_ysi(LOCfiles[5])
    LOC_6 <- process_ysi(LOCfiles[6])
    LOC_7 <- process_ysi(LOCfiles[7])
    LOC_8 <- process_ysi(LOCfiles[8])
    LOC_9 <- process_ysi(LOCfiles[9])
    LOC_10 <- process_ysi(LOCfiles[10])
    LOC_11 <- process_ysi(LOCfiles[11])
    LOC_12 <- process_ysi(LOCfiles[12])
    LOC_13 <- process_ysi(LOCfiles[13])
    LOC_14 <- process_ysi(LOCfiles[14])
    LOC_15 <- process_ysi(LOCfiles[15])
    LOC_16 <- process_ysi(LOCfiles[16])
    LOC_17 <- process_ysi(LOCfiles[17])
    LOC_18 <- process_ysi(LOCfiles[18])
    LOC_19 <- process_ysi(LOCfiles[19])
    LOC_20 <- process_ysi(LOCfiles[20])
    LOC_21 <- process_ysi(LOCfiles[21])
    

# 02 Visualize Profiles  ---------------------------------------------
    
  # GL4 
  Round_Plot_YSI_FUNC(GL4_1, 0.5) 
  Round_Plot_YSI_FUNC(GL4_2, 0.5) 
  Round_Plot_YSI_FUNC(GL4_3, 0.25)
  Round_Plot_YSI_FUNC(GL4_4, 0.25) 
  Round_Plot_YSI_FUNC(GL4_5, 0.25) # October, only one point at 0.25 m 
  Round_Plot_YSI_FUNC(GL4_6, 0.25)
  Round_Plot_YSI_FUNC(GL4_7, 0.25)
  Round_Plot_YSI_FUNC(GL4_8, 0.25)
  Round_Plot_YSI_FUNC(GL4_9, 0.25)
  Round_Plot_YSI_FUNC(GL4_10, 0.25)
  Round_Plot_YSI_FUNC(GL4_11, 0.25)
  Round_Plot_YSI_FUNC(GL4_12, 0.25)
  Round_Plot_YSI_FUNC(GL4_13, 0.5)
  
  
  # LOC 
  Round_Plot_YSI_FUNC(LOC_1, 0.25)
  Round_Plot_YSI_FUNC(LOC_2, 0.1) # Looks good 
  Round_Plot_YSI_FUNC(LOC_3, 0.25)
  Round_Plot_YSI_FUNC(LOC_4, 0.1)
  Round_Plot_YSI_FUNC(LOC_5, 0.25)
  Round_Plot_YSI_FUNC(LOC_6, 0.1)
  Round_Plot_YSI_FUNC(LOC_7, 0.25)
  Round_Plot_YSI_FUNC(LOC_8, 0.25)
  Round_Plot_YSI_FUNC(LOC_9, 0.25)
  Round_Plot_YSI_FUNC(LOC_10, 0.1) #October 2024 only one reading at 0.5 m 
  Round_Plot_YSI_FUNC(LOC_11, 0.1) # November 2024 only one two readings from around 0.6
  Round_Plot_YSI_FUNC(LOC_12, 0.1) # Beautiful!! 
  Round_Plot_YSI_FUNC(LOC_13, 0.1) # big gap but still looks okay at 0.1
  Round_Plot_YSI_FUNC(LOC_14, 0.1) #big gap but still looks okay at 0.1
  Round_Plot_YSI_FUNC(LOC_15, 0.1) # beautiful 
  Round_Plot_YSI_FUNC(LOC_16, 0.25) #yuck, gaps and all over the place, better with 0.25 but not awesome 
  Round_Plot_YSI_FUNC(LOC_17, 0.25) # gaps and kinda all over but not bad, looks better at 0.25
  Round_Plot_YSI_FUNC(LOC_18, 0.25) # gap between 2.5 and 3 m 
  Round_Plot_YSI_FUNC(LOC_19, 0.25) # regular gaps, honestly I think from uncoil and drop then slowly feed 
  Round_Plot_YSI_FUNC(LOC_20, 0.25) # kinda gross and jumpy with gaps 
  Round_Plot_YSI_FUNC(LOC_21, 0.25)
  
  
# 03 Export csv with rounded depths  ---------------------------------------------
  
  #GL4 
      GL4_20240627 <- GL4_1 %>%
        mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_20240627, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2024_06_27_profile.csv"))
    
      GL4_20240723 <- GL4_2 %>%
        mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv( GL4_20240723, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2024_07_23_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_3, 0.25) # good at 0.25
      GL4_20240822 <- GL4_3 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv( GL4_20240723, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2024_08_22_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_4, 0.25) # good at 0.25
      GL4_20240926 <- GL4_4 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv( GL4_20240926, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2024_09_26_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_6, 0.25)
      GL4_20241203 <- GL4_6 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_20241203, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2024_12_03_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_7, 0.25)
      GL4_2025_01_28 <- GL4_7 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv( GL4_2025_01_28, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/ GL4_2025_01_28_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_8, 0.25)
      GL4_2025_02_27 <- GL4_8 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_02_27, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_02_27_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_9, 0.25)
      GL4_2025_03_20 <- GL4_9 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_03_20, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_03_20_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_10, 0.25)
      GL4_2025_04_22 <- GL4_10 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_04_22, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_04_22_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_11, 0.25)
      GL4_2025_07_01 <- GL4_11 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_07_01, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_07_01_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_12, 0.25)
      GL4_2025_07_23 <- GL4_12 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_07_23, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_07_23_profile.csv"))
      
      Round_Plot_YSI_FUNC(GL4_13, 0.5)
      GL4_2025_08_12 <- GL4_13 %>%
        mutate(depth_m=round(depth_m/0.5)*0.5) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(GL4_2025_08_12, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/GL4/GL4_2025_08_12_profile.csv"))
      
  
  # LOC 
      Round_Plot_YSI_FUNC(LOC_1, 0.25)
      LOC_2023_12_05 <- LOC_1 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2023_12_05, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2023_12_05_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_2, 0.1) 
      LOC_2024_01_30 <- LOC_2 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_01_30, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_01_30_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_3, 0.25)
      LOC_2024_02_06 <- LOC_3 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_02_06, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_02_06_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_4, 0.1)
      LOC_2024_03_05 <- LOC_4 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_03_05, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_03_05_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_5, 0.25)
      LOC_2024_04_23 <- LOC_5 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_04_23, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_04_23_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_6, 0.1)
      LOC_2024_05_13 <- LOC_6 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_05_13, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/ LOC_2024_05_13_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_7, 0.25)
      LOC_2024_06_13 <- LOC_7 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_06_13, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_06_13_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_8, 0.25)
      LOC_2024_07_09 <- LOC_8 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_07_09, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_07_09_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_9, 0.25)
      LOC_2024_09_24 <- LOC_9 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_09_24, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_09_24_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_10, 0.1) #October 2024 only one reading at 0.5 m 
      Round_Plot_YSI_FUNC(LOC_11, 0.1) # November 2024 only one two readings from around 0.6
      
      Round_Plot_YSI_FUNC(LOC_12, 0.1)
      LOC_2024_12_12 <- LOC_12 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2024_12_12, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2024_12_12_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_13, 0.1) 
      LOC_2025_01_16 <- LOC_13 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_01_16, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_01_16_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_14, 0.1)
      LOC_2025_01_24 <- LOC_14 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_01_24, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_01_24_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_15, 0.1)
      LOC_2025_02_11 <- LOC_15 %>%
        mutate(depth_m=round(depth_m/0.1)*0.1) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_02_11, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_02_11_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_16, 0.25)
      LOC_2025_02_25 <- LOC_16 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_02_25, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_02_25_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_17, 0.25) 
      LOC_2025_03_11 <- LOC_17 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_03_11, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_03_11_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_18, 0.25)  
      LOC_2025_04_15 <- LOC_18 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_04_15, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_04_15_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_19, 0.25) 
      LOC_2025_05_22 <- LOC_19 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_05_22, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_05_22_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_20, 0.25) 
      LOC_2025_06_19 <- LOC_20 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_06_19, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_06_19_profile.csv"))
      
      Round_Plot_YSI_FUNC(LOC_21, 0.25)
      LOC_2025_07_29 <- LOC_21 %>%
        mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
        group_by(lake, date, depth_m, parameter) %>%
        summarize(value = median(value, na.rm=TRUE)) 
      write_csv(LOC_2025_07_29, here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/YSI_DSSPro/cleaned/LOC/LOC_2025_07_29_profile.csv"))
      
      
      
  
# # Profiles for The Loch (LOC) 
# 
# # Inspect the profiles, summarize the data, and export inot "LOC > export" folder
# 
# 
# # ... 2024-03-05
# 
# LOC1 <- process_ysi(LOCfiles[1])
# head(LOC1)
# 
# LOC1 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC1$lake),unique(LOC1$date)))
# 
# 
# # ... 2024-04-23 
# 
# LOC2 <- process_ysi(LOCfiles[2])
# head(LOC2)
# 
# LOC2 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC2$lake),unique(LOC2$date)))
# 
# 
# # ... 2024-05-13 
# 
# LOC3 <- process_ysi(LOCfiles[3])
# head(LOC3)
# 
# LOC3 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC3$lake),unique(LOC3$date)))
# 
# 
# # ... For dates with multiple holes, pick the best looking one and export that 
# LOC14 <- process_ysi(LOCfiles[14])
# head(LOC14)
# 
# LOC14 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC14$lake),unique(LOC14$date)))
# 
# LOC16 <- process_ysi(LOCfiles[16])
# head(LOC16)
# 
# LOC16 %>%
#   mutate(depth_m=round(depth_m/0.25)*0.25) %>% #round to the nearest 0.5
#   group_by(depth_m, parameter, lake) %>%
#   mutate(value = median(value, na.rm=TRUE)) %>%
#   mutate(month=month(date_time)) %>%
#   filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
#   ggplot(aes(x=value, y=depth_m, color=parameter))+
#   geom_point()+
#   scale_y_reverse()+
#   facet_wrap(parameter~., scales="free_x", nrow = 2)+
#   labs(title=paste(unique(LOC16$lake),unique(LOC16$date)))


# OTHER LAKES 


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
