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
    head(bb_data)

# NEED TO CONVERT TO %Saturation to look at 

# quick plot to check 
bb_data %>%
  ggplot(aes(x = date_time, y = do_obs, color = as.character(sensor_num), group = as.character(sensor_num))) + 
  geom_point()+ 
  geom_line() + 
  theme_minimal()
