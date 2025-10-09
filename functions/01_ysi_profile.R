# ----------------------------------------------------------------------------------
# This script was written by Adeline G. Kelly and simply takes the YSI Pro DSS 
# file and converts it into a CSV and make the headings all pretty. This 
# function is meant for looking at lake profiles. See script 02_ysi_point.R 
# for a point measurement function.
# ----------------------------------------------------------------------------------


process_ysi <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_]")[[1]]
  
  # Read in file
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  
  # Fix encoding and special characters in column names
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- gsub("<b5>", "µ", colnames(data))
  
  # Define rename map (old = new)
  rename_map <- c(
    "Date (MM/DD/YYYY)" = "date", 
    "Time (HH:mm:ss)" = "time", 
    "Chlorophyll RFU" = "chla_RFU", 
    "Cond µS/cm" = "cond_uScm", 
    "Depth m" = "depth_m", 
    "ODO % sat" = "do_percent", 
    "ODO mg/L" = "do_mgL", 
    "ORP mV" = "orp_mV", 
    "SpCond µS/cm" = "cond_spec_uScm", 
    "TAL PC RFU" = "phycoC_RFU", 
    "pH" = "pH", 
    "Temp °C" = "temp_C", 
    "Barometer mmHg" = "barometer_mmHg"
  )
  
  # Keep only names that exist in the data
  existing_rename_map <- rename_map[names(rename_map) %in% colnames(data)]
  
  # Safely rename columns
  data <- data %>% rename(!!!setNames(names(existing_rename_map), existing_rename_map))
  
  # Merge date and time if both exist
  if (all(c("date", "time") %in% colnames(data))) {
    data <- data %>%
      mutate(date_time = paste(date, time))
  } else {
    data$date_time <- NA
  }
  
  # Desired columns (only keep those that exist)
  desired_cols <- c("date_time", "chla_RFU", "cond_uScm", "depth_m", "do_percent", 
                    "do_mgL", "orp_mV", "cond_spec_uScm", "phycoC_RFU", 
                    "pH", "temp_C", "barometer_mmHg")
  keep_cols <- intersect(desired_cols, colnames(data))
  
  # Build final dataframe
  data <- data %>%
    select(all_of(c("date_time", keep_cols))) %>%
    mutate(lake = file_info[1],
           site = file_info[2]) %>%
    relocate(lake, .before = everything()) %>%
    relocate(site, .after = lake) %>%
    relocate(depth_m, .after = date_time) %>%
    mutate(date_time = suppressWarnings(mdy_hms(date_time)),
           date = as.Date(date_time)) %>%
    pivot_longer(cols = any_of(setdiff(keep_cols, c("date_time", "depth_m"))), 
                 names_to = "parameter")
  
  return(data)
}

# Write plotting function 
Round_Plot_YSI_FUNC <- function(ysi_profile, round_to_nearest ){
      ysi_profile %>%
        mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest ) %>% #round to the nearest 0.5
        group_by(depth_m, parameter, lake) %>%
        mutate(value = median(value, na.rm=TRUE)) %>%
        mutate(month=month(date_time)) %>%
        filter(!parameter %in% c("barometer_mmHg","cond_spec_uScm")) %>%
        ggplot(aes(x=value, y=depth_m, color=parameter))+
        geom_point()+
        scale_y_reverse()+
        facet_wrap(parameter~., scales="free_x", nrow = 2)+
        labs(title=paste(unique(ysi_profile$lake),unique(ysi_profile$date)))
    }

# Write a function that rounds, summarizes by depth, and pivots the data to wide format 
OTI_YSI_FUNC <- function(ysi_profile, round_to_nearest){

      # Round to the nearest depth (based on what you decided looking at the plots ) 
      ysi_profile_rounded <- ysi_profile %>%
        mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest ) 

      # Save the date time that the profile was collected as date 
      ysi_profile_rounded$date <- ysi_profile_rounded$date_time[1] # funky because we want to keep the date-time that the profile was taken but we don't want to summarize by time bc it would replicate for each second or minute and some of our profiles cover a lot of time 

      # Summarize: take median parameter value for each unique combination of lake, site, date, depth
      ysi_profile_summarized <- ysi_profile_rounded %>% #round to the nearest 0.5
        group_by(lake, site, date, depth_m, parameter) %>% # gather everythinf into groups correspond to a unique combination of lake, date, depth
        summarise(value = median(value, na.rm = TRUE), .groups = "drop") # take the median of each group 

      # Pivot the resulting table from long format to wide format 
      ysi_wide <- ysi_profile_summarized %>%
        select(lake, site, date, depth_m, parameter, value) %>%  # keep relevant columns
        pivot_wider(
          names_from = parameter,   # each unique parameter becomes its own column
          values_from = value       # fill those columns with the 'value' data
        )
       
      # Format columns and column names 

          # some columns just need to be renamed 
          names(ysi_wide)[names(ysi_wide) == "lake"] <- "lakeID" 
          names(ysi_wide)[names(ysi_wide) == "temp_C"] <- "temp_degC" 
          names(ysi_wide)[names(ysi_wide) == "do_mgL"] <- "doConcentration_mgpL" 
          names(ysi_wide)[names(ysi_wide) == "do_percent"] <- "doSaturation_percent"
          names(ysi_wide)[names(ysi_wide) == "cond_spec_uScm"] <- "specificConductivity_uSpcm" 

          # Format dates to be compatable 
          ysi_wide$date_yyyy.mm.dd <- as.Date(ysi_wide$date)
          ysi_wide$time_hhmmss <- format(ysi_wide$date, "%H:%M:%S")

          # Convert the units of barometric pressure to tbe same as the rest of the OTI Team 
          ysi_wide$waterPressure_barA <- ysi_wide$barometer_mmHg * 0.0013322 # we measure barometric pressure as barometer_mmHg, for "water pressure" (under water rather than in air handheld) Dave wanrs barA as the units 
          
          # Some parameters we don't collect on our instrument so give them a column with explicit NAs 
          ysi_wide$turbidity_FNU <- NA # explicit column of NAs for data that we do not have 
          ysi_wide$salinity_psu <- NA # explicit column of NAs for data that we do not have
          ysi_wide$tds_mgpL <- NA # explicit column of NAs for data that we do not have
          ysi_wide$barometerAirHandheld_mbars <- NA # explicit column of NAs for data that we do not have 

          # Set lat long and altitude based on lake 
          ysi_wide$latitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_lat, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_lat, NA)) 
          ysi_wide$longitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_long, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_long, NA))
          ysi_wide$altitude_m <- ifelse(ysi_wide$lakeID == "GL4", gl4_alt, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_alt, NA))

          # We have some timepoints for the loch where we have CHLA and PHYC but other time points when we don't. Set it up so that if we have data it populates and if not the column gets explocot NAs 
          ysi_wide <- ysi_wide %>% mutate(chlorophyll_RFU  = if ("chla_RFU" %in% names(.)) chla_RFU else NA) # take ysi_wide and make a new column called "chlorophyll_RFU" (what Dave wants this called), if the data frame includes a column named "chla_RFU" (what we name that column), then use the data from that column. If there is no column with that name (if we don't have that data) then fill the column with NAs 
          ysi_wide <- ysi_wide %>% mutate(phycocyaninBGA_RFU  = if ("phycoC_RFUU" %in% names(.)) phycoC_RFU else NA)
          ysi_wide <- ysi_wide %>% mutate(pH  = if ("pH" %in% names(.)) pH else NA) # also for some reason some timepoints with no pH and no orp
          ysi_wide <- ysi_wide %>% mutate(orp_mV = if ("orp_mV" %in% names(.)) orp_mV else NA)


      # Put all together into one nice formatted dataframe 
      ysi_clean <- subset(ysi_wide, select = c("lakeID" , "date_yyyy.mm.dd", "time_hhmmss", "depth_m", "temp_degC", "doConcentration_mgpL",
                                    "doSaturation_percent", "chlorophyll_RFU", "phycocyaninBGA_RFU", "turbidity_FNU", "pH", "orp_mV", 
                                    "specificConductivity_uSpcm", "salinity_psu", "tds_mgpL", "waterPressure_barA", "latitude",
                                      "longitude", "altitude_m", "barometerAirHandheld_mbars" ))

      return(ysi_clean)
    }


## ORIGINAL just in case
# process_ysi <- function(file_path) {
#   # Extract information from file name
#   file_name <- path_file(file_path)
#   file_info <- strsplit(file_name, "[_]")[[1]]
#   # Read in file
#   data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
#   # Fix encoding and special characters in column names
#   Encoding(colnames(data)) <- "latin1"
#   colnames(data) <- gsub("<b5>", "µ", colnames(data))
#   # Rename columns
#   # Call by column name rather than position
#   data <- data %>%
#     rename(
#       date = "Date (MM/DD/YYYY)", 
#       time = "Time (HH:mm:ss)", 
#       chla_RFU = "Chlorophyll RFU", 
#       cond_uScm = "Cond µS/cm", 
#       depth_m = "Depth m", 
#       do_percent = "ODO % sat", 
#       do_mgL = "ODO mg/L", 
#       orp_mV = "ORP mV", 
#       cond_spec_uScm = "SpCond µS/cm", 
#       phycoC_RFU = "TAL PC RFU", 
#       pH = "pH", 
#       temp_C = "Temp °C", 
#       barometer_mmHg = "Barometer mmHg"
#     ) %>%
#     # Merge date and time columns
#     mutate(date_time = paste(date, time)) %>%
#     # Select desired columns
#     select(date_time, chla_RFU, cond_uScm, depth_m, do_percent, do_mgL, orp_mV, cond_spec_uScm, 
#            phycoC_RFU, pH, temp_C, barometer_mmHg
#     ) %>%
#     # Create columns for lake and site from file path
#     mutate(lake = file_info[1],
#            site = file_info[2]) %>%
#     # Move columns
#     relocate(date_time, .before = chla_RFU) %>%
#     relocate(lake, .before = date_time) %>%
#     relocate(site, .before = date_time) %>%
#     relocate(depth_m, .after = date_time) %>%
#     # Fix date formatting
#     mutate(date_time = mdy_hms(date_time),
#            date = date(date_time)) %>%
#     # Pivot to long format
#     pivot_longer(cols = c(chla_RFU:barometer_mmHg), names_to = "parameter") 
#   # Return new dataframe
#   return(data)
# }

# Example below for profile measurement
# result_df <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250415.csv")
