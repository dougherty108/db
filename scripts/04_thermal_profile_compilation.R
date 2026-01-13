############ Thermal Profile Compilation Script ########

#Author: Charlie Dougherty

# Script Objective: the EXO, MiniDOTs, and Hobos all collect temperature data, 
# yet they are not available in one file. The product of this script will be a thermal 
# profile file that is cleaned for non-deployment times (i.e. the sensors were out of the water for service). 

# Starting with the Loch, then will branch out to the other lakes. 

# call the functions that process EXO, HOBO, and MiniDOT data
source("functions/07_EXO3.R")
source("functions/04_HOBO.R")
source("functions/03_miniDOT.R")


#Step 1. EXO
# define exo path, starting with the summer deployment from 2025
EXO_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/Sensors/YSI EXO3/Field Deployment"

# process file, we're only interested in temperature so only keep the depth sensor (because the EXO has one), datetime, and temperature
exo_temp = process_EXO(EXO_path) |> 
  select(date_time, depth_from_top, depth_from_bottom, lake_ID, depth_m_from_pressure, temp_C)


#Step 2. HOBOS
HOBO_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/Sensors/HOBO/LOC"
#load files
hobo_temp = compile_HOBO_data(filepath = HOBO_path)

#Step 3. MiniDOTS
miniDOT_path = ""
