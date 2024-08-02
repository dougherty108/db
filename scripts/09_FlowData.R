source("scripts/00_libraries.R")

# file extension is .dat; comma delimited text files 

# these will ultimately be uploaded as different files w the following naming scheme: 
  # "Loch_Inlet_299_T_10min_2024XXXX.dat" with readout date in unix format 

# I'm thinking it would make sense to write a script that searches the folder for files ending in .dat, 
  # similar to how miniDOT data are processed. 

# stored in Data > On Thin Ice > 09_FlowData > LOC_INLET > raw > files

# read in all 10 minute files, code all variables as numeric

# need to trim NAs before coding as numeric 

# read in df

inlet_10min <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/Loch_Inlet_299_T_10min.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_pt_C = 4, avg_cond = 5, ct_avg = 6, cond_temp_C = 7) %>%
  mutate(date_time = ymd_hms(date_time)) # %>%
  inlet_10min[3:7] <- lapply(inlet_10min[3:7], function(x) as.numeric(x))
# write function to remove all rows with NAS

# recode all columns containing data as numeric 
inlet_10min[3:7] <- lapply(inlet_10min[3:7], function(x) as.numeric(x))
str(inlet_10min)

# get rid of NAs 
# yeah this doesn't run whatever 
inlet_10min[3:7] <- lapply(inlet_10min[3:7], function(x) na.omit(x))

# draft code to read in multiple inlet files/compile 

sky_raw <- bind_rows((fs::dir_ls("Data/LVWS/05_miniDOT/SKY/raw/sky_0.5", regexp = "\\.txt$") %>%
    purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
    select(1, 3, 4) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
    mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`))), 


inlet_10min <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/Loch_Inlet_299_T_10min.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_pt_C = 4, avg_cond = 5, ct_avg = 6, cond_temp_C = 7) %>%
  mutate(date_time = ymd_hms(date_time))    
    
inlet_compile <-
  fs::dir_ls("Data/On Thin Ice/09_FlowData/LOC_INLET/raw", regexp = "\\.dat$") %>%
  purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 4, header = FALSE))




# have to skip first four lines to get it to read in - column names and units are as follows
# "TIMESTAMP","RECORD","Lvl_ft","Temp_C_PT_Avg","Cond_Avg","Ct_Avg","Temp_C_SCT_Avg"
# "TS","RN","feet","deg C","mS/cm","mS/cm","Deg C"
# need to clarify the difference between ct_avg and cond_average. units are the same, likely specific conductivity? 

# table 2 (daily? check field notebook AK)

inlet_dailytable <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/Loch_Inlet_299_Table2.dat", sep = ",", skip = 4, 
                           header = FALSE) %>%
  rename(date_time = 1, record = 2, battery_voltage = 3) %>%
  mutate(date_time = ymd_hms(date_time))

# one reading per day so it's ok that this doesn't include a time when it parses 

# this should be called daily table - I need to check if this is high or low battery voltage. 

# "TIMESTAMP","RECORD","BattV_Min"
# "TS","RN","Volts"

# datetime coding as character - fix























