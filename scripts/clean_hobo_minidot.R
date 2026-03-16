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
        source(here::here("functions", "hobo_functions.R")) 

    # -------- YOU NEED TO CHANGE ON YOUR MACHINE 
    # Connect to Sharepoint data connection on your machine 
        data_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/" # Katie's desktop


#___________________________________________
# MiniDOT 
#___________________________________________

# 1. Load and format data streams

    # Load and clean data using the "git miniDOT" function from the miniDOT functions script (loaded above)
    combined_data <- get_miniDOT(data_path) # this can take a minute, it is a lot of data to process 

    # Load in and format df of deployments and retreival times 
    # NOTE: you do not need to change this path because it is all within the repo 
    depret <- read_excel("data/meta_data/buoy_deployment_retreival_full_day.xlsx")
    depret_paired <- clean_deploy_retrieve(depret)


# 2. Create a "status" column in combined data to denote when the buoy was above water (between retreivals and deployments)
  # NOTE: this function can take some time because the "fuzzy join" is pretty slow checking every row of all your miniDOT data 
    minidot <- buoy_above_water(combined_data, depret_paired)

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


# 3.  Visually inspect the output 

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


# 4. Seperate the DO and the temp data into seperate dfs, temp will get added to hobo bellow 
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