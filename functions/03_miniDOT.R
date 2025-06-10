source("functions/00_libraries.R")
source("functions/00_helper_functions.R")

#Compile miniDOT data

# Define the path to the main directory
main_dir <- here("data/Sensors/miniDOT")

# Get all text files in the main directory and its subdirectories
files <- dir_ls(main_dir, regexp = "\\.txt$", recurse = TRUE)

# Filter out files from the "concat" folder
files <- files[!str_detect(files, "/concat/")]

# Filter out files from the "QC" folder
files <- files[!str_detect(files, "QC")]

#Ignore the "new data" folder -- migrating files into here at the moment IAO 2025-05-01
files <- files[!str_detect(files, "/new data/")]


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
# Start the clock!
ptm <- proc.time()
combined_data <- files %>%
  map_dfr(process_file) %>%
  mutate(salinity = 0) %>%
  group_by(lake_id,depth)
  #Currently, these do_sat values are sus and aren't a 1:1 match with the old
  #dataframe (code preserved below). Need to troubleshoot this - IAO 20240802
  # mutate(do_sat = case_when(lake_id=="loch" ~ 100 * do_obs/oxySol(temp, salinity, 0.68),
  #                           lake_id=="SKY" ~ 100 * do_obs/oxySol(temp, salinity, 0.66))) 
proc.time() - ptm


# View the combined data
print(combined_data)


# Append concatenated files for some early years in Sky Pond --------------
sky_concat <- bind_rows(read.table("data/Sensors/miniDOT/concat/Sky_6.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "SKY", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("data/Sensors/miniDOT/concat/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "SKY", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("data/Sensors/miniDOT/concat/Sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "SKY", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)),
  read.table("data/Sensors/miniDOT/concat/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "SKY", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`)) %>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.66)))#last term is atm pressure)
# 'corrected' hypolimnion depth to 6m. will look through field notebooks and fix later -AGK
# I change it to 6.5m, which was the depth from surface -IAO

# Append concatenated files for some early years in The Loch --------------

# concatenated files - pulled from an earlier version. keeping do_sat, can also remove and calculate manually
# have to convert date formatting in order to combine with dataframe of raw files
loch_concat <- bind_rows(read.table("data/Sensors/miniDOT/concat/Loch_4.5m_16-17_all.TXT", sep = ",", header = TRUE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "LOC", local_tz = "Mountain", daylight_savings = "Yes", depth = "4", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
      mutate(salinity = 0,
             do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("data/Sensors/miniDOT/concat/Loch_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "LOC", local_tz = "Mountain", daylight_savings = "Yes", depth = "1.5", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("data/Sensors/miniDOT/concat/Loch_hypo_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "LOC", local_tz = "Mountain", daylight_savings = "Yes", depth = "4", depth_from="TOP", folder_name="concat") %>%
    mutate(date_time = as_datetime(`date_time`))%>%
    mutate(salinity = 0,
           do_sat = 100 * do_obs/oxySol(temp, salinity, 0.68)),
  read.table("data/Sensors/miniDOT/concat/Loch_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(2, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "LOC", local_tz = "Mountain", daylight_savings = "Yes", depth = "1.5", depth_from="TOP", folder_name="concat") %>%
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

#eventually I want to be able to flag any intervals not included between any of the 
#start_time - end_time intervals in the metadata above. Currently doing this manually, not ideal -IAO

combined_data_clean <- combined_data %>%
  mutate(depth = as.numeric(depth),
         flag = case_when(lake_id == "LOC" & date_time < "2016-07-19 13:39:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2017-05-30 12:00:00" & date_time < "2017-07-14 13:38:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2017-09-27 13:00:00" & date_time < "2017-10-04 14:30:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2018-06-19 11:00:00" & date_time < "2018-06-26 16:30:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2018-09-11 10:01:00" & date_time < "2018-09-24 17:30:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2019-10-01 16:30:00" & date_time < "2019-10-15 09:00:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2020-05-26 13:41:00" & date_time < "2020-06-02 11:00:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2021-06-08 11:30:00" & date_time < "2021-06-15 10:00:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2022-08-09 13:00:00" & date_time < "2022-08-16 13:15:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2023-09-05 14:00:00" & date_time < "2023-09-28 13:07:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2024-06-13 09:30:00" & date_time < "2024-08-20 10:07:00" ~ "above water",
                          lake_id == "LOC" & date_time > "2024-10-17 9:00:00" ~ "above water",
                          lake_id == "SKY" & date_time < "2016-08-11 11:27:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2017-06-22 11:45:00" & date_time < "2017-07-13 09:30:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2017-09-27 10:30:00" & date_time < "2017-10-04 11:40:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2018-06-19 09:00:00" & date_time < "2018-06-26 12:00:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2018-08-25 09:00:00" & date_time < "2018-09-25 10:23:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2019-07-31 07:45:00" & date_time < "2019-08-06 08:30:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2020-07-14 09:00:00" & date_time < "2020-07-21 13:15:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2021-06-11 07:37:00" & date_time < "2021-07-06 10:08:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2021-06-11 07:37:00" & date_time < "2021-07-06 10:08:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2021-09-28 05:00:00" & date_time < "2021-09-28 11:26:00" ~ "above water",
                          #^^ IAO- i know this one is weird but we are missing data for summer 2021 for mid and hypo sensors
                          lake_id == "SKY" & date_time > "2022-08-09 09:45:00" & date_time < "2022-08-16 10:00:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2023-09-05 12:30:00" & date_time < "2023-09-19 09:15:00" ~ "above water",
                          lake_id == "SKY" & date_time > "2024-06-25 08:15:00" ~ "above water",
                          TRUE ~ "under water")) %>%
  mutate(logging_frequency = difftime(date_time, lag(date_time), units = "mins")) %>%
  # There are some years where it is clear the miniDOT logger was lodged in the ice. I want to flag these as well.
  # These cases are based on visual inspect and field notes.
  mutate(flag = case_when(lake_id == "SKY" & depth == "0.5" & date_time > "2020-10-01 00:00:00" & date_time < "2021-06-11 07:37:00" ~ "frozen",
                          TRUE ~ flag )) %>%
  mutate(depth_from_top = case_when(lake_id == "SKY" & depth_from == "BOT" ~ 7 - depth,
                                    lake_id == "SKY" & depth_from == "TOP" ~ depth,
                                    lake_id == "LOC" & depth_from == "BOT" ~ 5 - depth,
                                    lake_id == "LOC" & depth_from == "TOP" ~ depth,
                                    lake_id == "FER" & depth_from == "BOT" ~ 5.5 - depth,
                                    lake_id == "FER" & depth_from == "TOP" ~ depth),
         depth_from_bottom = ifelse(depth_from == "BOT", depth, NA_real_))

#Visual check the loch (color = flag)
combined_data_clean %>%
  filter(lake_id=="LOC" & flag == "under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  # filter(water_year == 2024) %>%
  # filter(date_time > "2024-08-20") %>%
  ggplot(aes(x=date_time, y=temp, color= factor(depth_from_top), shape=flag))+
  geom_point(alpha=0.5)+
  facet_wrap(year~., scales="free_x")+
  labs(title="The Loch")

#Visual check sky (color = depth, flags removed)
combined_data_clean %>%
  filter(lake_id=="SKY" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  # filter(temp < 20) %>%
  # filter(water_year %in% c('2024')) %>%
  ggplot(aes(x=date_time, y=temp, color=depth))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~., scales="free_x")

#Visual check sky (color = flag)
combined_data_clean %>%
  filter(lake_id=="SKY") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  # filter(temp < 20) %>%
  # filter(water_year %in% c('2024')) %>%
  ggplot(aes(x=date_time, y=temp, color=flag))+
  geom_point(alpha=0.5)+
  facet_wrap(water_year~depth, scales="free_x")+
  labs(title="Sky Pond")

#Visual check sky (color = depth, flags removed)
combined_data_clean %>%
  filter(lake_id=="SKY" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(do_obs < 20) %>%
  # filter(water_year %in% c('2024')) %>%
  ggplot(aes(x=date_time, y=do_obs, color=depth))+
  geom_point(alpha=0.1)+
  facet_wrap(water_year~., scales="free_x")

# What's going on in Sky Pond spring 2021?
combined_data_clean %>%
  filter(lake_id=="SKY" & flag =="under water") %>%
  mutate(year=year(date_time),
         date=date(date_time),
         doy_wy=hydro.day(date),
         water_year=calcWaterYear(date))%>%
  filter(do_obs < 20) %>%
  filter(water_year %in% c('2021')) %>%
  pivot_longer(temp:do_obs) %>%
  ggplot(aes(x=date_time, y=value, color=depth))+
  geom_point(alpha=0.1)+
  facet_wrap(name~., scales="free_x") 


combined_data_clean %>%
  filter(lake_id=="SKY") %>%
  filter(date_time > "2021-07-06" & date_time < "2021-09-28") %>%
  pivot_longer(temp:do_obs) %>%
  ggplot(aes(x=date_time, y=value, color=factor(depth)))+
  geom_point(alpha=0.1)+
  facet_wrap(name~depth, scales="free_y") 


#Export data for Bryan from The Loch.
# BGdata <- combined_data_clean %>%
#   filter(lake_id == "LOC" & flag == "under water") 
# 
# BGdata %>%
#   mutate(year=year(date_time),
#          date=date(date_time),
#          doy_wy=hydro.day(date),
#          water_year=calcWaterYear(date))%>%
#   ggplot(aes(x=date_time, y=temp, color=depth))+
#   geom_point(alpha=0.1)+
#   facet_wrap(~water_year, scales="free_x")
# 
# write_csv(BGdata, "data_export/loch_minidot_WY2017-2024.csv")

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




