######################################
# Katie Sensor Data Clean Up 
######################################
# this script conducts basic qa/qc and trimming of data from high frequency sensor to get data ready to be published on EDI 
# Katie Gannon 2026-02-26


# 0. Set Up R Environment 

    # Load packages and functions 
        library(here)
        source(here::here("functions", "00_libraries.R"))
        source(here::here("functions", "minidot_functions.R"))  
        # source(here::here("functions", "04_HOBO.R")) 

    # -------- YOU NEED TO CHANGE ON YOUR MACHINE 
    # Connect to Sharepoint data connection on your machine 
        data_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/" # Katie's desktop

    # Load and clean data using the "git miniDOT" function from the miniDOT functions script (loaded above)
    combined_data <- get_miniDOT(data_path) # this can take a minute, it is a lot of data to process 
    depret <- read_excel(file.path(data_path, "Sensors/buoy_deployment_retreival_test.xlsx"))


#___________________________________________
# MiniDOT 
#___________________________________________


##### Katie Scratch 2026-04-03 figuring out buoy deployment with sensor nuumber START ________________________________________
    # # Make a test dataset that is only sky 2025
    # combined_data <- combined_data %>% 
    #   filter(date_time >= as.POSIXct("2025-05-01") & lake_id == "SKY")

    # # plot to sanity check 
    # combined_data %>%
    #   ggplot(aes(x = date_time, y = temp, color = as.character(depth), group = as.character(depth))) + 
    #   geom_line() + 
    #   theme_minimal() + 
    #   facet_wrap(~lake_id)


#___________________________________________
# 1. Format the deployment and retreival data 

  # Format the deployment and retreival data 
    # format datetime into a timestamp POSIXct 
    depret$date <- paste(substring(depret$date_time, 1, 4), substring(depret$date_time, 5, 6), substring(depret$date_time, 7, 8), sep = "-" ) # this just adds "-" in between the year month and day in the date so that it is an unambiguous format and sets it to the very end of the day (removes the )

    # format date times to remove the full day when sensor was out of the water 
    depret$date_time <- ifelse(depret$deployed_retreived == "deployed", paste(depret$date, "23:59:59", sep = " "), 
                                  paste(depret$date, "00:00:01", sep = " "))
    depret$date_time  <- as.POSIXct(depret$date_time , format = "%Y-%m-%d %H:%M:%OS") # format the timestamp as a POSIXct 

    # Subset to only the sensor number, deploy retreive, and the time (then when you run through the minidot data just seperate everything by sensor number )
    depret <- subset(depret, select = c("sensor_number", "deployed_retreived", "date_time"))

    # pivot into wide format with a column for time deployed and the following columbn for time retreived 
    depret_paired <- depret %>%
      arrange(sensor_number, date_time) %>%  # order all of the rows by sensor number and then withiin sensor number arrange by date 
      group_by(sensor_number) %>%       # group within each sensor because we want to work through each sensor independently 
      mutate(
        event_id = cumsum(deployed_retreived == "deployed") # this creates a new column called "event_id" with a cumulative sum of all of the times for that sensor number that deployed_retreived column equals "deployed", essentially a count of each deployment. We need this in order to keep rows for each consecutive deployment and retreival pair 
      ) %>%
      pivot_wider(  # change format from long to wide 
        id_cols = c(sensor_number, event_id), # columns that stay the same <- importantly we need to include the event id here 
        names_from = deployed_retreived, # column whose values become new column names
        values_from = date_time, # what fills those new columns
        names_prefix = "time_" # add this prefix to the begining of the new column names 
      ) %>%
      filter(complete.cases(time_deployed, time_retreived)) %>% #this is a failsafe check so we are only keeping rows that have BOTH a time deployed AND a time retreived. This will get rid of rows where we have deployed the sensor in the lake but we haven't retreived it yet (so we shouldn't have data )
      ungroup()

#___________________________________________
# 2. Create a "status" column in combined data to denote when the buoy was above water (between retreivals and deployments)
  # NOTE: this function can take some time because the "fuzzy join" is pretty slow checking every row of all your miniDOT data 
    # minidot <- buoy_above_water(combined_data, depret_paired)

    # get rid of the first 5 digits of the sensor number code because they are all the same and we don't record them 
    combined_data$sensor_num <- substring(combined_data$sensor_num, 6, 14) 

    # seperate the combined minidot data into a list with a seperate data frame for each sensor number
    combined_data_lst <- split(combined_data, f = combined_data$sensor_num)

    # check that the function works 
      #  sand <- combined_data_lst[[3]]
      #  castle <- sensor_above_water(sensor_data = sand, depret_paired = depret_paired)

    # apply the sensor above water function across the list of combined data, adding an annotation for when the sensors were under water 
    combined_data_annotated_lst <- lapply(combined_data_lst, sensor_above_water, depret_paired)

    # clean up the output and put back together
    combined_data_annotated <- do.call(rbind, combined_data_annotated_lst)

    # plot to sanity check 
    combined_data_annotated %>%
      filter(sensor_status == "under_water") %>%
      ggplot(aes(x = date_time, y = temp, color = as.character(depth), group = as.character(depth))) + 
      geom_line() + 
      theme_minimal() + 
      facet_wrap(~lake_id)



#___________________________________________
# 3. Make a few formatting changes so that this data plays nice with other scripts 

minidot_reformatted <- minidot %>%
  rename(siteID = lake_id) %>% # for joining with SQL database. lake_id == siteID
  mutate(year = year(date_time), # extract year
        waterYear = calcWaterYear(date_time), # add water year column
        siteID = tolower(siteID), # make all sites lowercase
        depth = as.numeric(depth)) %>% #make depth a number for calculations below
  # IAO to KAG --> get rid of this once you add a std dev flag within the function. This still isn't catching
  # all the weirdness 
  mutate(do_flag = case_when(do_obs >= 100 ~ "unrealistically high value",
                                  TRUE ~ NA)) %>% # remove obvious outliers
  # Eventually this should reference zMax from a site metadata table. Fine for now.
  mutate(depth_from_top = case_when(siteID == "sky" & depth_from == "BOT" ~ 7 - depth,
                                    siteID == "sky" & depth_from == "TOP" ~ depth,
                                    siteID == "loc" & depth_from == "BOT" ~ 5 - depth,
                                    siteID == "loc" & depth_from == "TOP" ~ depth,
                                    siteID == "fer" & depth_from == "BOT" ~ 5.5 - depth,
                                    siteID == "fern" & depth_from == "TOP" ~ depth),
         depth_from_bottom = ifelse(depth_from == "BOT", depth, NA_real_),
        depth_from_top = as.character(depth_from_top))

#___________________________________________
# 4. add a flag column for values greater than 5 sd away from the mean 

    minidot_flagged <- minidot_reformatted %>%
      ungroup() %>% # remove any pre-existing grouping 
      mutate(
        month = month(date_time)
      ) %>%
      group_by(siteID, waterYear, depth_from_top, month) %>% # group by the factors that you care about 
      mutate(
        do_mean = mean(do_obs, na.rm = TRUE), # create a new column called do_mean with the average do for that group 
        do_sd   = sd(do_obs, na.rm = TRUE), # create another new column for sd 
        temp_mean = mean(temp, na.rm = TRUE), 
        temp_sd = sd(temp, na.rm = TRUE)
      ) %>%
      mutate(
        do_flag = case_when( # create a new column called do_flag, then use "case when" (similar to if else structure)
            abs(do_obs - do_mean) > 5 * do_sd ~ "outside_normal_range",# if the absolute value of the difference between the observed fo and the mean is greater than 5 * the sd then make that column say "outside of normal range"
            TRUE ~ "within_normal_range" # otherwise have that column say within normal ranve 
          ), 
        temp_flag = case_when(
          abs(temp - temp_mean) > 5 * temp_sd ~ "outside_normal_range", 
          TRUE ~ "within_normal_range"
        )
      )%>%
      ungroup() %>% # then ungroup everything again 
      select(-do_mean, -do_sd, -temp_mean, -temp_sd, -month) # remove the columns that you don't need anymore 

#___________________________________________
# 5.  Visually inspect the output 

    # plot do over time colored by status column 
    minidot_reformatted %>% 
      filter(siteID== "loc" ) %>%
      filter(is.na(do_flag)) %>%
      filter(status=="under_water") %>%
      ggplot(aes(x= date_time , y= do_obs, color = depth_from_top)) +
      geom_point(alpha=0.4) + 
      # geom_point() + 
      theme_bw() + 
      labs(x = "Date", y = "DO (mg/L)", 
          title =  "DO over time") + 
      facet_wrap(~ waterYear, scales = "free")

#___________________________________________
# 6. Seperate the DO and the temp data into seperate dfs, temp will get added to hobo bellow 
    # IAO - here what we should do instead is save a file with just the do_obs for each unique siteID.
    # Then keep the temperature data and combine it into one temperature dataframe along with the HOBO temperature data
    # so we have a "temperature_profiles.csv"

    # I only want to publish loc & sky through end of 2023 on EDI for now. But for internal use, let's compile everything
    # and have a separate script that reads in the minidit file, grabs sky & loc only, and then trims column names. 

# 5. Save the DO output file 

#write.csv(minidot_reformatted, file = "data/derived_data/minidot_edi.csv")


#___________________________________________
# HOBO 
#___________________________________________

    # Define the path to the hobo data directory within your data directory 
    hobo_data_path <- file.path(data_path, "Sensors/HOBO")


    # Get all csv files in the main directory and its subdirectories
      hobo_file_paths <- dir_ls(hobo_data_path, 
        regexp = "\\.csv$", 
        recurse = TRUE
      ) #This is taking every txt file in the "data path/sensors/miniDOT" folder recursively searching all of the foulders in that foulder
      
    # for each file path in your list apply this function 
    hobo_formatted <- lapply(hobo_file_paths, process_hobo_file)

    # Put all the hobo dfs together into one long temp profiles 
    temp_profile <- bind_rows(hobo_formatted)
    head(temp_profile)
    # Add the temp data from the minidots 
    

hobo_file_paths[5]