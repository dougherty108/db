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
    # bb_data <- get_miniDOT(data_path) # change the internal selection in get minidot to only pull the bubble bath 
bb_data <- read.csv("data_export/20260413_minidotbubble_bath.csv")
bb_data$date_time <- as.POSIXct(bb_data$date_time)
head(bb_data)

# NEED TO CONVERT TO %Saturation to look at # boulder is at 1655
bb_data$do_mgL_at_sat <-o2.at.sat.base(temp = bb_data$temp , altitude= 1655, salinity = bb_data$salinity)
bb_data$do_sat <- bb_data$do_obs / bb_data$do_mgL_at_sat
avg_do_sat <- mean(bb_data$do_mgL_at_sat)

# Plot to check 
bb_data %>%
    ggplot(aes(x = date_time, y = do_obs, color = as.character(sensor_num), group = as.character(sensor_num))) + 
    geom_point()+ 
    geom_line() + 
    geom_hline( # add horizontal dashed line 
                yintercept = avg_do_sat,
                linetype = "dashed",
                linewidth = 0.8,
                color = "gray40"
            ) +
    theme_minimal()

bb_dat_summary %>%
    ggplot(aes(x = date_time, y = do_sat, color = as.character(sensor_num), group = as.character(sensor_num))) + 
    geom_point()+ 
    geom_line() + 
    geom_hline( # add horizontal dashed line 
                yintercept = 1,
                linetype = "dashed",
                linewidth = 0.8,
                color = "gray40"
            ) +
    theme_minimal()

# Trim to only the period in the bucket
# 
bb_data$do_mgl_offset <-  bb_data$do_obs - bb_data$do_mgL_at_sat

bb_dat_summary <- bb_data %>%
    filter(date_time >= as.POSIXct("2026-04-13 10:00:00") & date_time <= as.POSIXct("2026-04-13 15:00:00")) %>%
    group_by(sensor_num) %>%
    summarise(
        mean_do_offset = mean(do_mgl_offset, na.rm = TRUE), 
    )

write.csv(bb_dat_summary, file = "data_export/minidot_sensor_offsets_20260413.csv")

# get the average difference from full saturation for each sensor 
# then you can save that number for each sentso on this calibration check day and use that to make corrections 
