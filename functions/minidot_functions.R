#######################
# miniDOT Functions 
#######################
# This script contains functions for working with miniDOT data for FAB project KAG 
# Copied from mtnlimnolab_db git hub: db / functions / 03_miniDOT and edited for specific workflow KAG 20260106
# additional functions added by K Gannon 

#########################
# process minidot file function 
#   GOAL: formats miniDOT file column names and creates columns based on the naming scheme of the folder where the file was saved 
#   input: file path to a single txt file of miniDOT data 
#   ouput: formatted df 

    process_minidot_file <- function(file_path, p) {

        p()  # signal one step of progress
 
        # Extract folder name
        folder_path <- dirname(file_path) # dirname returns the part of the path up to but excluding the last path separator, or "." if there is no path separator.
        folder_name <- path_file(folder_path) # THen this grabs only the last folder name  
        
        # Extract information from folder name (delimited by underscores)
        folder_info <- strsplit(folder_name, "_")[[1]] # not this says take that folder name and split it into seperate objects, based on the "_"
        
        # Read the data from the file
        data <- read.table(file_path, sep = ",", skip = 2, header = TRUE)

        # Extract the sensor number from the text file 
        sensor_number <- readLines(file_path, n = 1)
        
        # Add new columns based on the folder name
        data <- data %>%
          select(`Time..sec.`,`T..deg.C.`,`DO..mg.l.`) %>%
          dplyr::rename(date_time = `Time..sec.`,
                        temp = `T..deg.C.`,
                        do_obs = `DO..mg.l.`) %>%
          mutate(local_tz = "Mountain",
                daylight_savings = "Yes",
                date_time = as_datetime(as.numeric(date_time), tz = "UTC"), 
                # Convert from Unix time (in UTC)
                date_time = with_tz(date_time, tz = "America/Denver")) %>%            
          mutate(
            folder_name = folder_name,
            lake_id = folder_info[1],
            depth = folder_info[2],
            depth_from = folder_info[3],
            location = ifelse(length(folder_info) >= 4, folder_info[4], "zmax") #this is adding a little flexibility for when we have shore sites and the file path indicates which site they are at
                    # this last location line of code says "check the length of the folder info object, if it contains 4 or more objects then use the forth to designate the location column, otherwise, call it zmax"
          ) %>%
          select(-folder_name)  # remove the folder name column becuase you have extracted all of the information that you need 
        
        # Add the sensor name extracted from the first line of the text file 
        data$sensor_num <- sensor_number
        
        return(data)
    }

#########################
# get miniDot function
#    GOAL: pull each txt file from within the data structure, then processes it using the process file function above
#    input: file path to the folder containing all of the minidot data 
#    ouput: one large df containing the minidot data from all of the files within the folder structure 

    # Define the function 
    get_miniDOT <- function(data_path) {
      
      # Define the path to the miniDOT data directory within your data directory 
      main_dir <- file.path(data_path, "Sensors/miniDOT") # they have this defined with data path seperate from script path, so file path is where the script is but data path is where the data are? 
      
      # Get all text files in the main directory and its subdirectories
      files <- dir_ls(main_dir, regexp = "\\.txt$", recurse = TRUE) #This is taking every txt file in the "data path/sensors/miniDOT" folder recursively searching all of the foulders in that foulder
      
      # ADD in commands to skip certain folders 
        # Filter out files from the "concat" folder or from the duplicates folder 
      files <- files[!str_detect(files, "/concat/")]
      files <- files[!str_detect(files, "/duplicates not deleting just in case/")]
      files <- files[!str_detect(files, "/00_TEMPORARY_LOC_2526/")]

      files <- files[str_detect(files, "bubble_baths")]

      # files <- files[str_detect(files, "SKY")] # if you want to specify only one or a few lakes 
      
      # Apply the processing function to each file and combine the results
      combined_data <- progressr::with_progress({
        p <- progressr::progressor(steps = length(files))  # one step per file
        files %>%
          map_dfr(process_minidot_file, p = p) %>%
          mutate(salinity = 0) %>%
          group_by(lake_id, depth)
      })

      return(combined_data)
    }

# KATIE WORKING HERE ***********************************************************





 


#########################
# sensor_above_water function 
# GOAL: we want to make a new column in do_dat called "status" that denotes whether the sensor was above water or below water 
# NOTE: this function can take some time because the "fuzzy join" is pretty slow checking every row of all your miniDOT data
          



# then write a function to be applied over each data frame in that list 

# inside that function: 
sensor_above_water <- function(sensor_data, depret_paired){
    # inputs: combined data, combined data from one sensor (sensor_data)

    # first subset the dep ret data to only include the rows corresponding to that sensor 

      # extract the semsor number for the sensor you are working with 
      selected_sensor_number <- sensor_data[1, "sensor_num"] %>% as.character

      # subset depret to only the sensor number you want 
      depret_sub <- depret_paired %>%
          filter(sensor_number == selected_sensor_number)

    # then fuzzy join 
      sensor_data_flagged <- sensor_data %>% #note that this step is kind of slow 
            fuzzy_left_join( # we use "fuzzy join" here because for time deployed and time retreived you want to join when they are within a range not necessarily equivalent 
              depret_sub, # join combined data with the depret paired data and use left join because you want to keep all of the rows in combined data 
              by = c( # column name in sensor data = column name in depret data 
                "sensor_num" = "sensor_number", # lake id needs to match exactly 
                "date_time" = "time_deployed", # here the date time needs to be greater than the time deployed 
                "date_time" = "time_retreived" # the datetime needs to be less than the time retreived 
              ),
              match_fun = list(`==`, `>`, `<`) # this tells fuzzy join the equivalents you want to use for the four joins above 
            )

    # Use that combined df to create a "status" column with "above water" for any row that does not fall within a deployment window 
    sensor_data_annotated <- sensor_data_flagged %>%
      mutate(
        sensor_status = if_else(is.na(event_id), # then this is saying "if your event id column is NA then you are above water, otherwise you are under water". 
                      "above_water", # We are using the event id column because the depret_paired df has an "event_id" column but the combined do data doesn't so if you are a row that did not get matched with the depret df then you are not in the under water group 
                      "under_water")
      )

    # Clean up the ouput 
        # remove the extra rows that you created as part of this process
        sensor_data_annotated <- sensor_data_annotated %>%
          select(-event_id, -time_deployed, -time_retreived, -sensor_number) # this gets rid of the event id, time deployed, and time retreived columns 
  
  print(selected_sensor_number)
  return(sensor_data_annotated)

}




# OLD ---------------------------------------------------------------------------------------

#########################
# clean_deploy_retrieve function
#    GOAL: this formats the spreadsheet of buoy deployments and retreivals from long format (more compatible with data entry and intuitive) into wide paired format where it can be used to trim data 
#    input: raw data frame of deployments and retreivals (*** I want to automate this through excel similar to our sample inventory)
#    output: cleaned and paired wide format df of buoy deployments and retreivals ready to be used to trim buoy data to only time periods under water 


# clean_deploy_retrieve <- function(depret){
#     # format datetime into a timestamp POSIXct 
#       depret$timestamp <- paste( substring(depret$date_time, 1, 4), substring(depret$date_time, 5, 6), substring(depret$date_time, 7, 17), sep = "-" ) # this just adds "-" in between the year month and day in the date so that it is an unambiguous format 
#       depret$timestamp <- as.POSIXct(depret$timestamp, format = "%Y-%m-%d %H:%M:%OS") # format the timestamp as a POSIXct 
#       depret <- subset(depret, select = c("lake_id", "location", "deployed_retreived", "timestamp"))

#     # pivot into wide format with a column for time deployed and the following columbn for time retreived 
#     depret_paired <- depret %>%
#       arrange(lake_id, location, timestamp) %>%  # order rows by lake > site > timestamp  # this Groups rows by lake_id, then within each lake, it groups by site, then finally within each site, sorts by timestamp (earliest -> latest)
#       group_by(lake_id, location) %>% # group all of the rows by lake and site 
#       mutate(
#         event_id = cumsum(deployed_retreived == "deployed") # this creates a new column called "event_id" with a cumulative sum of all of the times for that lake_id and location that deployed_retreived column equals "deployed", essentially a count of each deployment. We need this in order to keep rows for each consecutive deployment and retreival pair 
#       ) %>%
#       pivot_wider( # change format from long to wide 
#         id_cols = c(lake_id, location, event_id), # columns that stay the same
#         names_from = deployed_retreived, # column whose values become new column names
#         values_from = timestamp, # what fills those new columns
#         names_prefix = "time_" # add this prefix to the begining of the new column names 
#       ) %>%
#       filter(complete.cases(time_deployed, time_retreived)) %>% #this is a failsafe check so we are only keeping rows that have BOTH a time deployed AND a time retreived. This will get rid of rows where we have deployed the sensor in the lake but we haven't retreived it yet (so we shouldn't have data )
#       ungroup()
  
#       return(depret_paired)
# }

#########################
# buoy_above_water function
# GOAL: we want to make a new column in do_dat called "status" that denotes whether the buoy was above water or below water 
# GAMEPLAN: 
# For each row in combined_data:
#     Match on lake_id
#     Match on location
#     Check if date_time falls between time_deployed and time_retreived
#     If it does -->  "under water"
#     If it does not --> "above water"

#     inputs: 
#             combineddata = minidot data compiled from all txt files, each row represents a timepoint when a do and temp measurement was taken, can include multiple lakes, years, and sites within the lake 
#             depret_paired = data frame of deployment and retreival times formatted into wide format with paired deployment and retreival times 
#     ouput : the same combined data file but now with an additional column called "status" denoting whether the buoy was above or below the water at that timepoint          
#
# NOTE: this function can take some time because the "fuzzy join" is pretty slow checking every row of all your miniDOT data
          
# buoy_above_water <- function(combined_data, depret_paired) {

#     # Merge the combined_do data with the depret_paired df 
#     combined_flagged <- combined_data %>% #note that this step is kind of slow 
#       fuzzy_left_join( # we use "fuzzy join" here because for time deployed and time retreived you want to join when they are within a range not necessarily equivalent 
#         depret_paired, # join combined data with the depret paired data and use left join because you want to keep all of the rows in combined data 
#         by = c(
#           "lake_id" = "lake_id", # lake id needs to match exactly 
#           "location" = "location", # location needs to match exactly 
#           "date_time" = "time_deployed", # here the date time needs to be greater than the time deployed 
#           "date_time" = "time_retreived" # the datetime needs to be less than the time retreived 
#         ),
#         match_fun = list(`==`, `==`, `>=`, `<=`) # this tells fuzzy join the equivalents you want to use for the four joins above 
#       )

#     # Use that combined df to create a "status" column with "above water" for any row that does not fall within a deployment window 
#     combined_flagged <- combined_flagged %>%
#       mutate(
#         status = if_else(is.na(event_id), # then this is saying "if your event id column is NA then you are above water, otherwise you are under water". 
#                       "above_water", # We are using the event id column because the depret_paired df has an "event_id" column but the combined do data doesn't so if you are a row that did not get matched with the depret df then you are not in the under water group 
#                       "under_water")
#       )

#     # Clean up the ouput 
#         # remove the extra rows that you created as part of this process
#         combined_flagged <- combined_flagged %>%
#           select(-event_id, -time_deployed, -time_retreived, -lake_id.y, -location.y) # this gets rid of the event id, time deployed, and time retreived columns 

#         # Rename the file names that got the .x at the end 
#         names(combined_flagged)[names(combined_flagged) == "lake_id.x" ] <- "lake_id"
#         names(combined_flagged)[names(combined_flagged) == "location.x" ] <- "location"
  
#   return(combined_flagged)
# }

# #########################
# # pivot_minidot_wide function <-  for metab, probably do not need here 
# # GOAL: Pivot minidot data so that rather than long format (with one column for depth, then one column for temp and one column for do) it is in wide format (with one columnb for temp at depth 1 then another column for temp at depth 2 and the same for do)
# # Input: minidot data in long format
# # Ouput: minidot data in wide format 

# pivot_minidot_wide <- function(minidot_long){

#     # split into just temp 
#     minidot_temp_long <- subset(minidot_long, select = c("lake_id", "date_time", "depth_sur", "temp"))

#     # pivot wide by temp 
#         minidot_temp_wide <- minidot_temp_long  %>%
#           pivot_wider( # change format from long to wide 
#             id_cols = c(lake_id, date_time), # columns that stay the same
#             names_from = depth_sur, # column whose values become new column names
#             values_from = temp, # what fills those new columns
#             values_fn = mean, # this means if you have multiple temp measurements at a given time step take the average 
#             names_prefix = "temp_" # add this prefix to the begining of the new column names 
#           )

#       # split into just do 
#         minidot_do_long <- subset(minidot_long, select = c("lake_id", "date_time", "depth_sur", "do_obs"))


#       # pivot wide by do 
#         minidot_do_wide <- minidot_do_long %>%
#                     pivot_wider( # change format from long to wide 
#                       id_cols = c(lake_id, date_time), # columns that stay the same
#                       names_from = depth_sur, # column whose values become new column names
#                       values_from = do_obs, # what fills those new columns
#                       values_fn = mean, # this means if you have multiple temp measurements at a given time step take the average 
#                       names_prefix = "do_" # add this prefix to the begining of the new column names 
#                     )

#       # put back together 
#       minidot_wide <- full_join(minidot_temp_wide, minidot_do_wide)
  
#   return(minidot_wide)

  # }