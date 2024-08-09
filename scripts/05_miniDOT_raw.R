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
    mutate(date_time = as_datetime(`date_time`, tz="America/Denver"),
           date_time = with_tz(date_time, tz="America/Denver")) %>%
    # IAO---I THINK that UTC is the default with the minidot software? as_datetime defaults to UTC.
    # I believe that "with_tz" changes how it prints, for our sanity
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
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("Data/LVWS/05_miniDOT/concat/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
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
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_hypo_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "4", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("Data/LVWS/05_miniDOT/concat/Loch_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "loch", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="T", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)))
# adjusted hypo depth to 4m, will fix after checking field notebooks -AGK
# corrected the hypo to 4.5m from surface - IAO

lvws_concat <- bind_rows(sky_concat, loch_concat) %>%
  mutate(date_time = as_datetime(`date_time`, tz="America/Denver"),
         date_time = force_tz(date_time, tz="America/Denver"),
         date_time = with_tz(date_time, tz="America/Denver"))


combined_data <- bind_rows(combined_data, sky_concat, loch_concat) %>%
  arrange(lake_id, depth, date_time) %>%
  distinct(date_time, .keep_all = TRUE)


# Filtering out times when the sensors were above water -------------------

metadata <- read_csv(here("data/LVWS/05_miniDOT/miniDot_metadata.csv")) %>%
  mutate(start_time=mdy_hm(start_time),
         end_time=mdy_hm(end_time),
         depth=as.character(as.numeric(depth)),
         depth_from=as.character(as.logical(depth_from)))

loch_metadata <- metadata %>%
  select(lake_id, start_time, end_time) %>%
  filter(lake_id=="loch")

sky_metadata <- metadata %>%
  select(lake_id, start_time, end_time) %>%
  filter(lake_id=="sky")
#eventually I want to be able to flag any intervals not included between any of the 
#start_time - end_time intervals in the metadata above. Currently doing this manually, not ideal -IAO

combined_data_clean <- combined_data %>%
  mutate(flag = case_when(lake_id == "loch" & date_time > "2017-05-30 10:00:00" & date_time < "2017-07-14 13:38:00" ~ "above water",
                          lake_id == "loch" & date_time > "2017-09-27 13:00:00" & date_time < "2017-10-04 14:30:00" ~ "above water",
                          lake_id == "loch" & date_time > "2018-06-19 11:00:00" & date_time < "2018-06-26 16:30:00" ~ "above water",
                          lake_id == "loch" & date_time > "2018-09-11 10:01:00" & date_time < "2018-09-24 13:50:00" ~ "above water",
                          lake_id == "loch" & date_time > "2019-10-01 16:30" & date_time < "2019-10-15 09:00" ~ "above water",
                          TRUE ~ "under water"))

ggplotly(combined_data_clean %>%
  filter(lake_id=="loch") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(temp < 20) %>%
  filter(year=='2023') %>%
  # filter(date_time > "2022-08-09")  %>%
  ggplot(aes(x=date_time, y=temp, color=flag))+
  geom_point(alpha=0.1)+
  facet_wrap(.~depth))




# Options for flagging data later -----------------------------------------

#IAO to AGK -- I found this resource on stackoverflow that I think should work
#for our data. I pasted the example code below. Give it a try? I also included
#2 more resources in case this doesn't work
#https://stackoverflow.com/questions/42371168/subset-time-series-by-groups-based-on-cutoff-date-data-frame

date <- seq(as.POSIXct("2014-07-21 17:00:00", tz= "GMT"), as.POSIXct("2014-09-11 24:00:00", tz= "GMT"), by="hour") 
group <- letters[1:4]             
# group <- rep(letters[1:4],2)             
datereps <- rep(date, length(group))                  
attr(datereps, "tzone") <- "GMT"
sitereps <- rep(group, each = length(date))    
value  <- rnorm(length(datereps))
df <- data.frame(DateTime = datereps, Group = group, Value = value)  


start <- c("2014-08-01 00:00:00 GMT", "2014-07-26 00:00:00 GMT", "2014-07-21 17:00:00 GMT", "2014-08-03 24:00:00 GMT")
end <- c("2014-09-11 24:00:00 GMT", "2014-09-01 24:00:00 GMT", "2014-09-07 24:00:00 GMT", "2014-09-11 24:00:00 GMT")
cut <- data.frame(Group = group, Start = as.POSIXct(start), End = as.POSIXct(end))

df2 <- merge(x = df,y = cut,by = "Group")
df2$flagvar <- !(df2$DateTime <= df2$Start | df2$DateTime >= df2$End)

#Does it work the way I think it should?
df2 %>%
  ggplot(aes(x=DateTime, y=Value, color=flagvar))+
  geom_point()+
  facet_wrap(~Group)
#Yes!

#Then in practice we change just filter out the 'FALSE' values
df2 %>%
  filter(flagvar=="TRUE") %>%
  ggplot(aes(x=DateTime, y=Value))+
  geom_point()+
  facet_wrap(~Group)

#https://business-science.github.io/timetk/reference/between_time.html 
# ^^ this might work for us too?

#https://stackoverflow.com/questions/64295796/use-dplyr-to-subset-time-series-data-from-specified-start-and-stop-times
## ^^ this could work but havent tried






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



