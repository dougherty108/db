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
    mutate(lake = file_info[1],
           site = file_info[2],
           date = date(date_time)) %>%
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

# Example below for profile measurement
# result_df <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250415.csv")
