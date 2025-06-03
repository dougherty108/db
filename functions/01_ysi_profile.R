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
