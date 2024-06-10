source("scripts/00_libraries.R")

# script to read in and process loch inlet data. 
  # I believe these are comma delimited text files but I'm not 100% sure 
# comma delimited text file, read.table works 
# extension is .dat

# stored in Data > On Thin Ice > 09_FlowData > LOC_INLET > raw > files

# starting with 10 minute data table 

inlet_10min <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/Loch_Inlet_299_T_10min.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_pt_C = 4, avg_cond = 5, ct_avg = 6, cond_temp_C = 7) %>%
  mutate(date_time = ymd_hms(date_time))

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























