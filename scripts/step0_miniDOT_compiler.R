######################################
# MiniDOT compiler 
######################################
# This script was original written by Katie Gannon
# and it conducts basic qa/qc and trimming of data from high frequency sensor 


# 0. Set Up R Environment 
# Load packages and functions 
library(here)
source(here::here("functions", "00_libraries.R"))
source(here::here("functions", "minidot_functions.R"))  

# Load in necessary metadata 
# Deployments and retreival times 
# TODO: this metadata file will be located in Data/Sensors in the future
# and will have additional columns not included here
depRet <- read_excel("data/meta_data/buoy_deployment_retreival_full_day.xlsx")

# Site metadata
# TODO: add site metadata table that has the lakeIDs and maximum depths 
siteMetadata <- read.csv()

# Set path to your local path to the cloud where core MLL "Data" folder lives
# data_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/" # Katie's desktop
# TODO: build in a data_path function into the miniDOT function

#___________________________________________
# Load and format data streams
#___________________________________________

# Load and clean data using the "git miniDOT" function from the miniDOT functions script (loaded above)
combined_data <- get_miniDOT(data_path) # this can take a minute, it is a lot of data to process 


depRet_paired <- clean_deploy_retrieve(depRet)


# Create a "status" column in combined data to denote when the buoy was above water (between retreivals and deployments)
# NOTE: this function can take some time because the "fuzzy join" is pretty slow checking every row of all your miniDOT data 
minidot <- buoy_above_water(combined_data, depRet_paired)

# Make a few formatting changes so that this data plays nice with other scripts 

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
# TODO : change the static depth values to actual depths from the siteMetadata table
mutate(depth_from_top = case_when(siteID == "sky" & depth_from == "BOT" ~ 7 - depth,
                                    siteID == "sky" & depth_from == "TOP" ~ depth,
                                    siteID == "loc" & depth_from == "BOT" ~ 5 - depth,
                                    siteID == "loc" & depth_from == "TOP" ~ depth,
                                    siteID == "fer" & depth_from == "BOT" ~ 5.5 - depth,
                                    siteID == "fern" & depth_from == "TOP" ~ depth),
         depth_from_bottom = ifelse(depth_from == "BOT", depth, NA_real_),
        depth_from_top = as.character(depth_from_top))

# add a flag column for values greater than 5 sd away from the mean 

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


# TODO: Seperate the DO and the temp data into seperate dfs, temp will get added to hobo in Step 1 downstream 

# Save the miniDOT DO output file 

# Save the miniDOT temperature output file
#write.csv(minidot_reformatted, file = "data/derived_data/minidot_edi.csv")
