source("scripts/00_libraries.R")

# processes YSI data - reads in file, renames columns, selects desired columns, returns new dataframe
process_ysi <- function(file_path) {
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- gsub("<b5>", "µ", colnames(data))
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
    select(
      date, time, chla, cond, depth, do_percent, do_mgL, orp, specific_cond, 
      TAL_PC, pH, temp, barometer
    )
  return(data)
}

# example with Andrew's Tarn below
result <- process_ysi("Data/LVWS/01_YSI/AND_TARN/raw/AndrewsTarn_Outlet_20240924.csv")









































