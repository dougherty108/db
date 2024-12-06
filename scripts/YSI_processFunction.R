source("scripts/00_libraries.R")

# need to write columns with site name, etc. from file path
# merge date and time columns

# processes YSI data - reads in file, renames columns, selects desired columns, returns new dataframe
process_ysi <- function(file_path) {
  
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_]")[[1]]
  
  # Read in file
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  
  # Fix encoding and special characters in column names
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- gsub("<b5>", "µ", colnames(data))
  
  # Rename and select columns
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
    select(date, time, chla, cond, depth, do_percent, do_mgL, orp, specific_cond, 
      TAL_PC, pH, temp, barometer
    ) %>%
    mutate(lake = file_info[1]) %>%
    mutate(site = file_info[2]) %>%
    relocate(lake, .before = date) %>%
    relocate(site, .before = date) %>%
    mutate(date = mdy(date)) %>%
    mutate(time = hms(time))
  
  return(data)
}



# example with Andrew's Tarn below
result_df <- process_ysi("Data/LVWS/01_YSI/AND_TARN/raw/AndrewsTarn_Outlet_20240924.csv")

# figure out pulling info from file name 

file_name <- path_file("Data/LVWS/01_YSI/AND_TARN/raw/AndrewsTarn_Outlet_20240924.csv")
file_info <- strsplit(file_name, "[_]")[[1]]

# date and time need to be parsed, relocate lake and site




































