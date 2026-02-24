######### script for processing LiCOR data outputs #########
source('functions/00_libraries.R')
# Author: Charlie Dougherty
filepath = '~/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/Sensors/LiCOR LI-1500/GL4/GL4_Zmax_20251216.TXT'
startdepth = 0.0
enddepth = 7.0
interval = 1.0

process_LICOR_file = function(filepath, interval, startdepth, enddepth) {
  
  obj <- read_tsv(filepath, skip = 8) # read in file, and skip down to where the data is
  
  obj_temp <- obj |> 
    rename(UWPAR_avg_µmolsec = "INPUT1", 
           UWPAR_min_µmolsec = "INPUT1_MIN", 
           UWPAR_max_µmolsec = "INPUT1_MAX", 
           AIRPAR_avg_µmolsec = "INPUT2", 
           AIRPAR_min_µmolsec = "INPUT2_MIN", 
           AIRPAR_max_µmolsec = "INPUT2_MAX") 
  
  last_row <- obj_temp$AIRPAR_avg_µmolsec[nrow(obj_temp)] # extract the last row of the dataframe, for the SW_out (upward radiating)
  
  obj_temp <- obj_temp[-nrow(obj_temp), ] # then remove the last row, we'll add the value back in in a wide format later
  

  obj_temp$depth_m <- c(startdepth, seq(1,enddepth, by = interval)) # add in depth values for the column
  
  obj_temp = obj_temp |> 
    mutate(AIRPAR_ice_µmolsec = last_row) |> 
    select(c(Date, Time, depth_m, UWPAR_avg_µmolsec, UWPAR_min_µmolsec, UWPAR_max_µmolsec, AIRPAR_avg_µmolsec, 
           AIRPAR_min_µmolsec, AIRPAR_max_µmolsec, AIRPAR_ice_µmolsec))
  final_par = obj_temp
  }

process_LICOR_file(filepath, interval, startdepth, enddepth)
