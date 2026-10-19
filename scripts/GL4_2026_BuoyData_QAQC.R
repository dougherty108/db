#########################################
# GL4 Buoy Data 2026 Raw --> EDI 
#########################################
# Katie Gannon 20260820 

# This script is written to QAQC data from the GL4 Niwot Buoy for 2025-2026 and to formatt the data to be ready to go up on EDI 
# Formatting should match all previous years of data for each 
# note that in order to access the EDI data from previous years you will need to set up the new EDI token trouble shoot 

#___________________________________________
# 0. Set Up R Environment 
#___________________________________________

    # Load packages and functions 
        source(here::here("functions", "00_libraries.R"))
        # source(here::here("functions", "minidot_functions.R"))  
        # source(here::here("functions", "04_HOBO.R")) 

    # -------- YOU NEED TO CHANGE ON YOUR MACHINE 
    # Connect to Sharepoint data connection on your machine 
        # data_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/" # Katie's desktop
        data_path_local <- "/Users/kaga3666/data_local/GL4_2026_data"

#___________________________________________
# 0. MiniDOT Data 
#___________________________________________


# NEXT KATIE: Go in to the minidot functions script, pull it apart and use/alter that code to make the miniDOT data that Bryan sent you look like the other years do on EDI 
# Remember that to access the EDI data you need to do the token access thing 