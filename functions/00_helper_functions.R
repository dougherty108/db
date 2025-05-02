#Function for calculating water-year DOY. This will help facilitate plotting and analysizing trends in ice-in since they span either side of the winter-year (e.g., 2011-2012). For example, an IceInDayofYear_fed value of 150 means Ice-In occured 150 days after the start of the water-year (Oct1)

hydro.day = function(x, start.month = 10L) {
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}


source("scripts/00_libraries.R")

process_ysi <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_]")[[1]]
  # Read in file
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  # Fix encoding and special characters in column names
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- gsub("<b5>", "µ", colnames(data))
  # Rename columns
  # Call by column name rather than position
  data <- data %>%
    rename(
      date = "Date (MM/DD/YYYY)", 
      time = "Time (HH:mm:ss)", 
      chla = "Chlorophyll RFU", 
      cond = "Cond µS/cm", 
      depth = "Depth m", 
      do_percent = "ODO % sat", 
      do_mgL = "ODO mg/L", 
      orp = "ORP mV", 
      specific_cond = "SpCond µS/cm", 
      TAL_PC = "TAL PC RFU", 
      pH = "pH", 
      temp = "Temp °C", 
      barometer = "Barometer mmHg"
    ) %>%
    # Merge date and time columns
    mutate(date_time = paste(date, time)) %>%
    # Select desired columns
    select(date_time, chla, cond, depth, do_percent, do_mgL, orp, specific_cond, 
           TAL_PC, pH, temp, barometer
    ) %>%
    # Create columns for lake and site from file path
    mutate(lake = file_info[1]) %>%
    mutate(site = file_info[2]) %>%
    # Move columns
    relocate(date_time, .before = chla) %>%
    relocate(lake, .before = date_time) %>%
    relocate(site, .before = date_time) %>%
    relocate(depth, .after = date_time) %>%
    # Fix date formatting
    mutate(date_time = mdy_hms(date_time)) %>%
    # Pivot to long format
    pivot_longer(cols = c(chla:barometer), names_to = "parameter") 
  # Return new dataframe
  return(data)
}

# Example below
# result_df <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Inlet_20240709.csv")

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

# result_df <- process_par("Data/On Thin Ice/07_PARsensors/LOC/raw/LOC_1.5_BOT_20240815_20241017.CSV")
