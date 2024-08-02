source("scripts/00_libraries.R")

# files are separated by depth, one per HOBO. writes all date_times to the same file - no need to manually concatenate

# read in HOBO file, add lake_id and depth
# select date_time, temp, light; rename
  # calling columns to select and rename by name rather than position 
# fix date formatting - parse as mdy_hms 

# will update this per miniDOT code to pull lake ID and depth from parent folders 

sky_hobo <- bind_rows(read.csv("Data/LVWS/06_HOBO/SKY/raw/Sky_1m 2024-06-25 09_39_31 MDT (Data MDT).csv") %>%
    select(Date.Time..MDT., Temperature.....C., Light....lux.) %>%
    rename(date_time = Date.Time..MDT., temp_C = Temperature.....C., light_lux = Light....lux.) %>%
    mutate(date_time = mdy_hms(date_time)) %>%
    mutate(lake_id = "SKY") %>%
    mutate(depth = "1m"), 
  read.csv("Data/LVWS/06_HOBO/SKY/raw/Sky_2m 2024-06-25 09_39_31 MDT (Data MDT).csv") %>%
    select(Date.Time..MDT., Temperature.....C., Light....lux.) %>%
    rename(date_time = Date.Time..MDT., temp_C = Temperature.....C., light_lux = Light....lux.) %>%
    mutate(date_time = mdy_hms(date_time)) %>%
    mutate(lake_id = "SKY") %>%
    mutate(depth = "2m"))
  
  
  







sky_hobo2 <- read.csv("Data/LVWS/06_HOBO/SKY/raw/Sky_2m 2024-06-25 09_43_46 MDT (Data MDT).csv")






































