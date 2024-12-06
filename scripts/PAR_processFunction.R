source("scripts/00_libraries.R")

# Odyssey PAR data processing - reads in file, renames columns, selects columns, writes to new dataframe
process_par <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_/]")[[1]]
  
  # Read in data from CSV file
  data <- read.csv(file_path, sep = ",", skip = 8, header = FALSE, strip.white = TRUE)
  
  # Rename and select columns
  data <- data %>%
  rename("scan_number" = 1, "date" = 2, "time" = 3, "raw_value" = 4, "calibrated_value" = 5) %>%
  mutate(date_time = paste(date, time)) %>%
  mutate(date_time = dmy_hms(date_time)) %>%
  relocate(date_time, .before = raw_value) %>%
  select(date_time, raw_value, calibrated_value) %>%
  mutate(lake = file_info[1]) %>%
  mutate(depth = file_info[2]) %>%
  mutate(depth_from = file_info[3])
  
  return(data)
  
}

result <- process_par("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.CSV")

# Write to new CSV file - include as part of function? 
# Need to think about file naming - name CSV file from file path? 

# Visualize PAR data










































