# ----------------------------------------------------------------------------------
# This script was written by Adeline G. Kelly and simply takes the YSI Pro DSS 
# file and converts it into a CSV and make the headings all pretty. This 
# function is meant for looking at a single point (e.g., stream). See script 
# 02_ysi_profile.Rfor a lake profile function.
# ----------------------------------------------------------------------------------


source("scripts/00_libraries.R")

# Function to calculate point estimates for YSI scripts - inlets, outlets, etc.

point_ysi <- function(file_path) {
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
    # Select desired columns
    select(date, chla, cond, do_percent, do_mgL, orp, specific_cond, 
      TAL_PC, pH, temp, barometer
    ) %>%
    # Create columns for lake and site from file path
    mutate(lake = file_info[1]) %>%
    mutate(site = file_info[2]) %>%
    # Move columns
    relocate(date, .before = chla) %>%
    relocate(lake, .before = date) %>%
    relocate(site, .before = date) %>%
    # Fix date formatting
    mutate(date = mdy(date)) %>%
    # Trim first 30 seconds 
    slice(-(1:30)) %>%
    # Group by lake, site, and date 
    group_by(lake, site, date) %>%
    # Calculate mean values 
    summarise(across(chla:barometer, mean)) %>%
    # Fix output structure
    ungroup() %>%
    as.data.frame()
  # Return new dataframe
  return(data)
}

test_function <- point_ysi("Data/LVWS/01_YSI/AND_TARN/raw/AndrewsTarn_Outlet_20240924.csv")
str(test_function)



















