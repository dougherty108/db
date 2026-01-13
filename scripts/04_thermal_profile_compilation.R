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
  mutate(depth_from_top = 5-depth_from_bottom, 
         sensor_type = "EXO") |> 
  select(date_time, depth_from_top, temp_C, lake_ID, sensor_type)


#Step 2. HOBOS
HOBO_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/Sensors/HOBO/LOC"
#load files
hobo_temp = compile_HOBO_data(filepath = HOBO_path) |> 
  mutate(depth_from_top = 5-depth_from_bottom, 
         temp_C = temperature_C, 
         lake_ID = "LOC") |> 
  select(date_time, depth_from_top, temp_C, lake_ID) |> 
  mutate(sensor_type = "HOBO")

#Step 3. MiniDOTS
miniDOT_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data"

#fun fact about this function, if you don't specify the water year it just returns everything
# it does take forever to run
miniDOT_temp = get_miniDOT(file_path = miniDOT_path, lake = "LOC")

DOT_temp = miniDOT_temp |> 
  mutate(temp_C = temp, 
         lake_ID = lake_id,
         depth_from_top = depth, 
         sensor_type = "MiniDOT") |> 
  select(date_time, depth_from_top, temp_C, lake_ID, sensor_type) |> 
  as_tibble() |> 
  select(-c(lake_id, depth))

#step 4. combine data into the same dataframe. 
#note, it's important here that the objects here all have the same structure. 
# structure should be datetime, depth, temp_c, and sensor type (what sensor it was collected on)
print(str(exo_temp))
print(str(hobo_temp))
print(str(as_tibble(DOT_temp)))

# bind temp data together
temp_profile = rbind(exo_temp, hobo_temp, DOT_temp) |> 
  filter(temp_C < 20)

#NOTE: this is not a QA/QC'd set of data above. This still needs cleaning

ggplot(temp_profile, aes(date_time, temp_C, color = as.character(depth_from_top))) + 
  geom_path() + 
  scale_color_brewer(palette = "Spectral") + 
  theme_bw()

# after looking at the data, there are multiple different sets of data that are close in depth, but not exactly the same. 
# need to group these together into smaller groups

temp_grouped = temp_profile |> 
  mutate(depth = depth_from_top) |> 
  mutate(depth_group = case_when(
    depth < 0.61                ~'0.50', 
    depth >=1.16 & depth < 1.26 ~'1.25', 
    depth == 1.50                ~'1.50', 
    depth == 2.00                ~'2.00', 
    depth == 2.25                ~'2.25',
    depth == 2.70                ~'2.75',
    depth >= 3.00 & depth < 3.11~'3.00',
    depth == 3.25                ~'3.25', 
    depth == 3.50                ~'3.50',
    depth == 3.83                ~'3.75',
    depth == 4.00                ~'4.00',
    depth == 4.25                ~'4.25'
  )) |> 
  mutate(water_year = calcWaterYear(date_time)) |> 
  drop_na(water_year)

ggplot(temp_grouped, aes(date_time, temp_C, color = as.character(depth_group))) + 
  geom_line() + 
  scale_color_brewer(palette = "Spectral") + 
  facet_wrap(vars(water_year), scales = "free") + 
  theme_bw()


