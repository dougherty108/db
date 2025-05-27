
process_EXO <- function(file_path) {
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
      total_algae_PC = 5,
      total_algae_PE = 6, 
      date = "Date (MM/DD/YYYY)",
      time = "Time (HH:mm:ss)",
      time_frac_sec = "Time (Fract. Sec)",
      site_name = "Site Name",
      cond_uScm = "Cond µS/cm",
      cond_nonlinear_uScm = "nLF Cond µS/cm",
      cond_spec_uScm = "SpCond µS/cm",
      depth_m = "Depth m",
      do_percent = "ODO % sat",
      do_percent_calibrated = "ODO % CB", # "Local DO value calculated from the barometric pressure value entered at the time of calibration."
      do_mgL = "ODO mg/L",
      TAL_PC_RFU = "TAL PC RFU",
      TAL_PE_RFU = "TAL PE RFU",
      TDS_mgL = "TDS mg/L",
      temp_C = "Temp °C",
      pressure_psia = "Pressure psi a",
      salinity_ppt = "Sal psu",
      vertical_position_m = "Vertical Position m",
      battery_V = "Battery V",
      cable_power_V = "Cable Pwr V"
    ) %>%
    # Merge date and time columns
    mutate(date_time = paste(date, time),
           date_time = mdy_hms(date_time)) 
  #   # Select desired columns
  #   select(date_time, chla, cond, depth, do_percent, do_mgL, orp, specific_cond, 
  #          TAL_PC, pH, temp, barometer
  #   ) %>%
    # Create columns for lake and site from file path
  #   mutate(lake = file_info[1]) %>%
  #   mutate(site = file_info[2]) %>%
  #   # Move columns
  #   relocate(date_time, .before = chla) %>%
  #   relocate(lake, .before = date_time) %>%
  #   relocate(site, .before = date_time) %>%
  #   relocate(depth, .after = date_time) %>%
  #   # Pivot to long format
  #   pivot_longer(cols = c(chla:barometer), names_to = "parameter") 
  # Return new dataframe
  return(data)
}

result_df <- process_EXO("data/Sensors/YSI EXO3/winter 2024 to spring 2025 deployment/Loch_EXO_20241024_20250430.csv")

names(result_df)

result_df %>%
  ggplot(aes(x=date_time, y=total_algae_PE))+
  geom_point()

result_df %>%
  ggplot(aes(x=total_algae_PE, y=TAL_PE_RFU))+
  geom_point(alpha=0.1)
