source("scripts/00_libraries.R")

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
  mutate(lake = file_info[1]) %>%
  mutate(depth = file_info[2]) %>%
  mutate(depth_from = file_info[3]) %>%
  # Move columns
  relocate(lake, .before = date_time) %>%
  relocate(depth, .after = date_time) %>%
  relocate(depth_from, .after = depth)
  # Return new dataframe
  return(data)
  
}

result_df <- process_par("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.CSV")









































