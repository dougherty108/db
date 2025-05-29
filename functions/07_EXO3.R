
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
      depth_m_from_pressure = "Depth m",
      do_percent = "ODO % sat",
      do_percent_calibrated = "ODO % CB", # "Local DO value calculated from the barometric pressure value entered at the time of calibration."
      do_mgL = "ODO mg/L",
      phycoE_RFU = "TAL PC RFU",
      phycoC_RFU = "TAL PE RFU",
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
           date_time = mdy_hms(date_time),
           date_time = force_tz(as_datetime(`date_time`), tzone = "America/Denver")) %>%
  #   # Select desired columns
  #   select(date_time, chla, cond, depth, do_percent, do_mgL, orp, specific_cond, 
  #          TAL_PC, pH, temp, barometer
  #   ) %>%
  # Create columns for lake and site from file path
    mutate(lake_id = file_info[1],
           depth_m = file_info[3],
           depth_from = file_info[4],
           depth_m = suppressWarnings(as.numeric(str_remove(depth_m, "m"))),
           deployment_date = ymd(file_info[5]),
           retrieval_date = ymd(file_info[6])) %>%
    #Convert depth from bottom to depth from top
    mutate(depth_from_top = case_when(lake_id == "LOC" & depth_from == "BOT" ~ 5 - depth_m,
                                      lake_id == "LOC" & depth_from == "TOP" ~ depth_m),
           depth_from_bottom = ifelse(depth_from == "BOT", depth_m, NA_real_)) %>%
  #   # Move columns
    relocate(date_time, .before = date) %>%
    relocate(lake_id, .after = date_time) %>%
    relocate(depth_from_top, .after = date_time) %>%
    relocate(depth_from_bottom, .after = depth_from_top)
  #   # Pivot to long format
  #   pivot_longer(cols = c(chla:barometer), names_to = "parameter") 
  # Return new dataframe
  return(data)
}

# result_df <- process_EXO("data/Sensors/YSI EXO3/winter 2024 to spring 2025 deployment/Loch_EXO_20241024_20250430.csv")

