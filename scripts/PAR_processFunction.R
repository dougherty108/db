# script to process Odyssey PAR data

# change specified test file path to "file_path", write into a function 

# header has to be false/can't preserve column names because they're on two different rows
par_test <- read.csv("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.csv", 
                     sep = ",", skip = 8, header = FALSE, strip.white = TRUE) %>%
  rename("scan_number" = 1, "date" = 2, "time" = 3, "raw_value" = 4, "calibrated_value" = 5) %>%
  mutate(date_time = paste(date, time)) %>%
  mutate(date_time = dmy_hms(date_time)) %>%
  relocate(date_time, .before = raw_value) %>%
  select(date_time, raw_value, calibrated_value)

file_path <- path_ext_remove("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.csv")
file_info <- strsplit(file_path, "[_/]")[[1]]

# lake = 7 
# depth = 8
# depth_from = 9

par_test <- par_test %>%
  mutate(lake = file_info[7]) %>%
  mutate(depth = file_info[8]) %>%
  mutate(depth_from = file_info[9])

# function 

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
















