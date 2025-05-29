source("functions/00_libraries.R")

# Odyssey PAR data processing - reads in file, renames columns, selects columns, writes to new dataframe
process_par <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_/]")[[1]]
  # Read in file
  data <- read.csv(file_path, sep = ",", skip = 8, header = FALSE, strip.white = TRUE)
  # Rename columns
  data <- data %>%
    rename("scan_number" = 1, "date" = 2, "time" = 3, "raw_value" = 4, "calibrated_value" = 5) %>%
    # Create date_time column, fix date formatting
    mutate(date_time = paste(date, time)) %>%
    mutate(date_time = dmy_hms(date_time)) %>%
    # Select columns
    select(date_time, raw_value, calibrated_value) %>%
    # Create columns for site and depth from file name
    mutate(lake_id = file_info[1],
           depth_m = file_info[2],
           depth_m = suppressWarnings(as.numeric(str_remove(depth_m, "m"))),
           depth_from = file_info[3],
           deployment_date = file_info[4],
           retrieval_date = file_info[5],
           serial_number = file_info[6],
           serial_number = suppressWarnings(str_remove(serial_number, ".CSV")),
           serial_number = suppressWarnings(as.numeric(str_remove(serial_number, "Serial")))) %>%
    # Move columns
    relocate(lake_id, .before = date_time) %>%
    relocate(depth_m, .after = date_time) %>%
    relocate(depth_from, .after = depth_m) %>%
    # Make sure the date times are right
  mutate(date_time = force_tz(as_datetime(`date_time`), tzone = "America/Denver")) %>%
    #Convert depth from bottom to depth from top
    mutate(depth_from_top = case_when(lake_id == "LOC" & depth_from == "BOT" ~ 5 - depth_m,
                                      lake_id == "LOC" & depth_from == "TOP" ~ depth_m),
           depth_from_bottom = ifelse(depth_from == "BOT", depth_m, NA_real_)) 
  # Return new dataframe
  return(data)
  
}

# result_df <- process_par("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.CSV")



