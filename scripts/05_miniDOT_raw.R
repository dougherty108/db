source("scripts/00_libraries.R")
source("scripts/00_functions.R")

#Compile miniDOT data for Sky, Loch, Fern

# Define the path to the main directory
main_dir <- here("Data/LVWS/05_miniDOT/")

# Get all text files in the main directory and its subdirectories
files <- dir_ls(main_dir, regexp = "\\.txt$", recurse = TRUE)

# Filter out files from the "concat" folder
files <- files[!str_detect(files, "/concat/")]

# Filter out files from the "FERN" folder
files <- files[!str_detect(files, "/FERN/")]


# Define a function to process each file
process_file <- function(file_path) {
  # Extract folder name
  folder_path <- dirname(file_path)
  folder_name <- path_file(folder_path)
  
  # Extract information from folder name (delimited by underscores)
  folder_info <- strsplit(folder_name, "_")[[1]]
  
  # Read the data from the file
  data <- read.table(file_path, sep = ",", skip = 2, header = TRUE)
  
  # Add new columns based on the folder name
  data <- data %>%
    select(`Time..sec.`,`T..deg.C.`,`DO..mg.l.`) %>%
    dplyr::rename(date_time = `Time..sec.`,
                  temp = `T..deg.C.`,
                  do_obs = `DO..mg.l.`) %>%
    mutate(local_tz = "Mountain", daylight_savings = "Yes") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(
      folder_name = folder_name,
      lake_id = folder_info[1],
      depth = folder_info[2],
      depth_from = folder_info[3]
    )
  
  return(data)
}

# Apply the function to each file and combine the results
combined_data <- files %>%
  map_dfr(process_file) %>%
  mutate(salinity = 0) %>%
  group_by(lake_id,depth) %>%
  #Currently, these do_sat values are sus and aren't a 1:1 match with the old
  #dataframe (code preserved below). Need to troubleshoot this - IAO 20240802
  mutate(do_sat = case_when(lake_id=="loch" ~ 100 * do_obs/oxySol(temp, salinity, 0.68),
                            lake_id=="sky" ~ 100 * do_obs/oxySol(temp, salinity, 0.66)))

# View the combined data
print(combined_data)


# Append concatenated files for some early years in Sky Pond --------------
sky_concat <- bind_rows(read.table("Data/LVWS/05_miniDOT/concat/Sky_6.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)))#last term is atm pressure)
# 'corrected' hypolimnion depth to 6m. will look through field notebooks and fix later -AGK
# I change it to 6.5m, which was the depth from surface -IAO

# Append concatenated files for some early years in The Loch --------------

# concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
# have to convert date formatting in order to combine with dataframe of raw files
loch_concat <- bind_rows(read.table("Data/LVWS/05_miniDOT/concat/Loch_4.5m_16-17_all.TXT", sep = ",", header = TRUE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_hypo_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)))
# adjusted hypo depth to 4m, will fix after checking field notebooks -AGK
# corrected the hypo to 4.5m from surface - IAO


combined_data2 <- bind_rows(combined_data, sky_concat, loch_concat) %>%
  arrange(lake_id, depth, date_time)


# Loch visual inspection --------------------------------------------------


combined_data2 %>%
  filter(lake_id=="loch") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  ggplot(aes(x=doy_wy, y=do_obs, color=folder_name))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~depth, scales="free_x")

#Potentially some overlap in the beginning of WY 2018?
combined_data2 %>%
  filter(lake_id=="loch") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(water_year=="2018")%>%
  ggplot(aes(x=doy_wy, y=do_obs, color=folder_name))+
  geom_point(alpha=0.1)+
  facet_wrap(folder_name~depth)

#Does it go away if we only have distint date_times?
combined_data2 %>%
  filter(lake_id=="loch") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(water_year=="2018")%>%
  distinct(date_time, .keep_all = TRUE) %>%
  ggplot(aes(x=doy_wy, y=do_obs, color=folder_name))+
  geom_point(alpha=0.1)+
  facet_wrap(folder_name~depth)
#Nope, must be real? 

# Sky visual inspection --------------------------------------------------

combined_data2 %>%
  filter(lake_id=="sky" & do_obs<12) %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  ggplot(aes(x=doy_wy, y=do_obs, color=folder_name))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~depth, scales="free_x")

#Potentially some overlap in WY 2018?
combined_data2 %>%
  filter(lake_id=="sky" & do_obs<12) %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(water_year=="2018")%>%
  ggplot(aes(x=date_time, y=do_obs, color=temp))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~depth)+
  scale_color_distiller(palette ="YlOrRd", direction=1,
                       guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))
#Need to check my  old field notes to see if the DO drawdowns are real or correpsond w sensors coming out?- IAO

combined_data2 %>%
  filter(lake_id=="sky" & do_obs<12) %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(water_year=="2018")%>%
  ggplot(aes(x=date_time, y=temp, color=folder_name))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~depth)
#Uh yeaaaaahh need to do a bit of data trimming before Katie uses these - IAO
#Stopped here 20240802 IAO


# OLD SCRIPTS below -- delete when we trust the new one - IAO -------------------



#sky pond database####
#raw files first, manually calculating do_sat

# thinking about shortening file paths? either by writing to a variable or defining a global variable

# this runs! -AGK 20240801
# sky_raw <- bind_rows((fs::dir_ls("Data/LVWS/05_miniDOT/sky_0.5_T", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`))), 
#   fs::dir_ls("Data/LVWS/05_miniDOT/sky_3.5_T", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "3.5") %>%
#     mutate(date_time = as_datetime(`date_time`)),
#   fs::dir_ls("Data/LVWS/05_miniDOT/sky_6_T", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
#     mutate(date_time = as_datetime(`date_time`))) %>%
#   mutate(salinity = 0,
#          do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66))#last term is atm pressure
#Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html

#concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
#have to convert date formatting in order to combine with dataframe of raw files

# concatenated files still need to be moved & these paths need to be updated once we settle on a file structure -AGK
# sky_concat <- bind_rows(read.table("Data/LVWS/05_miniDOT/SKY/concat/2016_17_SkyLH/Sky_6.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
#     mutate(date_time = as_datetime(`date_time`)), 
#   read.table("Data/LVWS/05_miniDOT/SKY/concat/2016_17_SkyLS/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`)), 
#   read.table("Data/LVWS/05_miniDOT/SKY/concat/Sky/2017_18_SkyLH/sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6") %>%
#     mutate(date_time = as_datetime(`date_time`)),
#   read.table("Data/LVWS/05_miniDOT/SKY/concat/Sky/2017_18_SkyLS/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`)))
# 'corrected' hypolimnion depth to 6m. will look through field notebooks and fix later -AGK

#build database
# sky_minidot <- bind_rows(sky_raw, sky_concat)

# #loch database####
# #raw files first, manually concatenating do_sat
# loch_raw <- bind_rows((fs::dir_ls("Data/LVWS/05_miniDOT/loch_0.5_T", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`))), 
#   fs::dir_ls("Data/LVWS/05_miniDOT/loch_4_T", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
#     mutate(date_time = as_datetime(`date_time`))) %>%
#   mutate(salinity = 0,
#          do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)) #last term is atm pressure
# #Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html
# 
# LVWS_combined <- bind_rows(sky_raw, loch_raw) %>%
#   mutate(dataset = "old")



#this is generating some NA values when converting unix to date_time. not sure why, will fix -AGK

#Check where the NAs are:
# loch_minidot %>% filter(if_any(everything(), is.na))
# Did not find any NAs with the function above so just overriding date_time with date_time

#concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
#have to convert date formatting in order to combine with dataframe of raw files
# loch_concat <- bind_rows(read.table("Data/LVWS/05_miniDOT/LOCH/concat/2016_17_LochLH/Loch_4.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
#     mutate(date_time = as_datetime(`date_time`)), 
#   read.table("Data/LVWS/05_miniDOT/LOCH/concat/2016_17_LochLS/Loch_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`)), 
#   read.table("Data/LVWS/05_miniDOT/LOCH/concat/2017_18_LochLH/Loch_hypo_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4") %>%
#     mutate(date_time = as_datetime(`date_time`)),
#   read.table("Data/LVWS/05_miniDOT/LOCH/concat/2017_18_LochLS/Loch_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
#     select(3, 5:7) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
#     mutate(lake_id = "Loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`)))
#adjusted hypo depth to 4m, will fix after checking field notebooks -AGK

#build database
# 
# loch_minidot <- bind_rows(loch_raw, loch_concat) %>%
#   mutate(month = month(date_time)) %>%
#   mutate(year = year(date_time)) %>%
#   mutate(year = as.factor(year)) %>%
#   mutate(month = as.factor(month))
# str(loch_minidot)

#fern database####
# fern_minidot <- bind_rows((fs::dir_ls("Data/LVWS/05_miniDOT/FERN/FERN_LS", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
#     mutate(date_time = as_datetime(`date_time`))), 
#   fs::dir_ls("Data/LVWS/05_miniDOT/FERN/FERN_LH", regexp = "\\.txt$") %>%
#     purrr::map_dfr( ~ read.table(.x, sep = ",", skip = 2, header = TRUE)) %>%
#     select(1, 3, 4) %>%
#     dplyr::rename(date_time = 1, temp = 2, do_obs = 3) %>%
#     mutate(lake_id = "Fern", local_tz = "Mountain", daylight_savings = "Yes", depth = "5") %>%
#     mutate(date_time = as_datetime(`date_time`)))%>%
#   mutate(salinity = 0,
#          do_sat = 100 * do_obs/oxySol(temp, salinity, 0.7)) #last term is atm pressure
#Get atm from elevation here: https://www.waterontheweb.org/under/waterquality/dosatcalc.html

# file paths updated, this runs. need to trim based on pull date/time -AGK



