source("scripts/00_libraries.R")

# file extension is .dat; comma delimited text files 

# Data > On Thin Ice > 09_FlowData > LOC_INLET > raw > T_10min
# T_10min stores actual data (i.e. 10 minute readings)

# 1) build database of all existing data (read in all files, bind into one dataframe)
# 2) write to csv file
# 3) write function that reads in csv file (10 min database), reads in file to be added, 
  # modifies file structure, and appends to inlet database
# 4) data QC? sensor types, what reads what? 

# build database ---------------------------------------------------------------
# read in existing files
# 20240610 
inlet_240610 <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20240610.dat", sep = ",", skip = 4,
                          header = FALSE) %>% # read in file - .dat, comma delimited text file
  # rename columns - skipping first fews due to file structure. names pulled from full file. 
  # could drop the record column - essentially just a row number
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  # fix date formatting
  mutate(date_time = ymd_hms(date_time)) 
# recode all columns containing data as numeric 
inlet_240610[3:7] <- lapply(inlet_240610[3:7], function(x) as.numeric(x))

# 20240730 
inlet_240730 <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20240730.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  mutate(date_time = ymd_hms(date_time)) 
inlet_240730[3:7] <- lapply(inlet_240730[3:7], function(x) as.numeric(x))

# 20240924 
inlet_240924 <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20240924.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  mutate(date_time = ymd_hms(date_time)) 
inlet_240924[3:7] <- lapply(inlet_240924[3:7], function(x) as.numeric(x))

# 20250321 
inlet_250321 <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20250321.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  mutate(date_time = ymd_hms(date_time)) 
inlet_250321[3:7] <- lapply(inlet_250321[3:7], function(x) as.numeric(x))

#inlet_db <- bind_rows(inlet_240610, inlet_240730, inlet_240924, inlet_250321)
inlet_db <- bind_rows(inlet_240610, inlet_240730, inlet_240924)

# write full dataframe to csv
write_csv(inlet_db, "Data/On Thin Ice/09_FlowData/LOC_INLET/export/inlet_db.csv")

# data adding function ---------------------------------------------------------
# read in existing database 
inlet_db1 <- read.csv("Data/On Thin Ice/09_FlowData/LOC_INLET/export/inlet_db.csv") %>%
  # fix date formatting, not preserved with exporting
  mutate(date_time = ymd_hms(date_time)) 

# read in new data - same as above
# will need to modify this when writing into a function such that it's file name agnostic
new_data <- read.table("Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20250321.dat", sep = ",", skip = 4,
                          header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  mutate(date_time = ymd_hms(date_time)) 
# recode all columns containing data as numeric 
new_data[3:7] <- lapply(new_data[3:7], function(x) as.numeric(x))
str(new_data)

# bind to dataframe of existing data
inlet_db2 <- bind_rows(inlet_db1, new_data)

# write function 
# input == file_path, read in file with specs above, read in existing db, append input data 

# inputs are file1 and file2 - file1 == existing inlet database, file2 == data to be added
inlet_function <- function(file1, file2) {
  # read in existing database file
  inlet_db1 <- read.csv(file1) %>%
  mutate(date_time = ymd_hms(date_time))
  # read in new data and modify dataframe
  new_data <- new_data <- read.table(file2, sep = ",", skip = 4, header = FALSE) %>%
  rename(date_time = 1, record = 2, level_feet = 3, temp_c_pt_avg = 4, avg_cond = 5, ct_avg = 6, temp_c_sct_avg = 7) %>%
  mutate(date_time = ymd_hms(date_time)) 
  # recode all columns containing data as numeric 
  new_data[3:7] <- lapply(new_data[3:7], function(x) as.numeric(x))
  # combine into one dataframe
  test_inlet <- bind_rows(inlet_db1, new_data)
  write_csv(test_inlet, "Data/On Thin Ice/09_FlowData/LOC_INLET/export/inlet_full.csv")
}

inlet_function("Data/On Thin Ice/09_FlowData/LOC_INLET/export/inlet_db.csv", "Data/On Thin Ice/09_FlowData/LOC_INLET/raw/T_10min/Loch_Inlet_299_T_10min_20250321.dat")




# plot data --------------------------------------------------------------------
inlet_10min_db %>%
  ggplot(aes(x = date_time, y = temp_c_pt_avg)) + 
  geom_point() + 
  geom_line()

inlet_10min_db %>%
  ggplot(aes(x = date_time, y = avg_cond)) + 
  geom_point() + 
  geom_line()

inlet_10min_db %>%
  ggplot(aes(x = date_time, y = ct_avg)) + 
  geom_point() + 
  geom_line()

inlet_10min_db %>%
  ggplot(aes(x = date_time, y = temp_c_sct_avg)) + 
  geom_point() + 
  geom_line()

inlet_10min_db %>%
  ggplot(aes(x = date_time, y = level_feet)) + 
  geom_point() + 
  geom_line()


















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























