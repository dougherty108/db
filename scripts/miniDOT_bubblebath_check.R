#########################################
# MiniDOT bubble bath code 
##########################################
# Code to check the calibraiton of miniDOTs following bubble bath 

# 0. Set Up R Environment 

    # Load packages and functions 
        library(here)
        source(here::here("functions", "00_libraries.R"))
        source(here::here("functions", "minidot_functions.R"))  

 # -------- YOU NEED TO CHANGE ON YOUR MACHINE 
    # Connect to Sharepoint data connection on your machine 
        data_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/" # Katie's desktop

# Load and clean data using the "git miniDOT" function from the miniDOT functions script (loaded above)
bb_data <- get_miniDOT(data_path) # change the internal selection in get minidot to only pull the bubble bath 
# bb_data <- read.csv("data_export/20260413_minidotbubble_bath.csv")
bb_data$date_time <- as.POSIXct(bb_data$date_time)
head(bb_data)

# NEED TO CONVERT TO %Saturation to look at # boulder is at 1655
bb_data$do_mgL_at_sat <-o2.at.sat.base(temp = bb_data$temp , altitude= 1655, salinity = bb_data$salinity)
bb_data$do_sat <- bb_data$do_obs / bb_data$do_mgL_at_sat
avg_do_sat <- mean(bb_data$do_mgL_at_sat)

# set to only most recent bubble bath 
bb_data_20260413 <- bb_data %>%
  filter(date_time <= as.POSIXct("2026-04-20"))

bb_data_20260422 <- bb_data %>%
  filter(date_time >= as.POSIXct("2026-04-20"))

# Plot to check 
bb_data_20260422 %>%
  filter(date_time >= as.POSIXct("2026-04-22 12:00:00") & date_time <= as.POSIXct("2026-04-23 9:00:00")) %>%
  ggplot(aes(x = date_time, y = do_sat, color = as.character(sensor_num), group = as.character(sensor_num))) + 
  geom_point()+ 
  geom_line() + 
  theme_minimal()

# Plot to check 
bb_data_20260413 %>%
  filter(date_time >= as.POSIXct("2026-04-13 09:30:00") & date_time <= as.POSIXct("2026-04-13 15:00:00")) %>%
  ggplot(aes(x = date_time, y = do_obs, color = as.character(sensor_num), group = as.character(sensor_num))) + 
  geom_point()+ 
  geom_line() + 
  theme_minimal()

# plot the two time series together 

gap_start <- as.POSIXct("2026-04-13 15:00:00")
gap_end   <- as.POSIXct("2026-04-22 12:00:00")

gap_length <- gap_end - gap_start

bb_data %>%
  filter(!(date_time >= gap_start & date_time <= gap_end)) %>%
  mutate(
    plot_time = ifelse(
      date_time > gap_end,
      date_time - gap_length,
      date_time
    ),
    plot_time = as.POSIXct(plot_time, origin = "1970-01-01")
  ) %>%
  ggplot(aes(
    x = plot_time,
    y = do_sat,
    color = as.character(sensor_num),
    group = as.character(sensor_num)
  )) +
  geom_point() +
  geom_line() +
  theme_minimal()
