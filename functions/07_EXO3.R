### NOTES
#01/13/2026 -- CD rewrote function to handle multiple files at once, instead of only processing one file at a time. Used the HOBO
# processing script to do this, as it handles multiple files quite well. 

process_EXO <- function(filepath) {
  files = list.files(filepath, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # warning to tell if you there are no files found in directory. This usually means you are pointing to the wrong directory
  if (length(files) == 0) {
    warning("No CSV files found in the specified directory.")
    return(NULL)
  }
  
  # Extract metadata
  metadata <- tibble(File = files) %>%
    mutate(
      FileBase = basename(File),
      Parts = str_split(FileBase, "_"),  
      lake_ID = map_chr(Parts, 1, .default = NA),  
      depth_m = map_chr(Parts, 3, .default = NA),
      depth_from = map_chr(Parts, 4, .default = NA),
      date_deployed = map_chr(Parts, 5, .default = NA),
      date_retrieved = map_chr(Parts, 6, .default = NA),
      sensor_type = map_chr(Parts, 2, .default = NA)
    ) %>%
    select(File, lake_ID, depth_m, depth_from, date_deployed, date_retrieved, sensor_type) %>%
    mutate(depth_m = suppressWarnings(as.numeric(str_remove(depth_m, "m"))))  
  
  file = files[[1]]
  
  # Read and clean files
  data_list <- map(files, function(file) {
    tryCatch({
      data <- read.csv(file, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
      
      file_info <- strsplit(basename(file), "[_]")[[1]]
      
      Encoding(colnames(data)) <- "latin1"
      colnames(data) <- gsub("<b5>", "µ", colnames(data))
      
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
        mutate(lake_ID = file_info[1],
               depth_m = file_info[3],
               depth_from = file_info[4],
               depth_m = suppressWarnings(as.numeric(str_remove(depth_m, "m"))),
               deployment_date = ymd(file_info[5]),
               retrieval_date = ymd(file_info[6]), 
               File = file_info[1]) %>%
        #Convert depth from bottom to depth from top
        mutate(depth_from_top = case_when(lake_ID == "LOC" & depth_from == "BOT" ~ 5 - depth_m,
                                          lake_ID == "LOC" & depth_from == "TOP" ~ depth_m),
               depth_from_bottom = ifelse(depth_from == "BOT", depth_m, NA_real_)) %>%
        #   # Move columns
        relocate(date_time, .before = date) %>%
        relocate(lake_ID, .after = date_time) %>%
        relocate(depth_from_top, .after = date_time) %>%
        relocate(depth_from_bottom, .after = depth_from_top)
      
      
      return(data)
    }, error = function(e) {
      warning(paste("Skipping file:", file, "\nError:", e$message))
      return(NULL)
    })
  })
  
  data_list <- compact(data_list)
  
  if (length(data_list) == 0) {
    warning("No valid CSV files could be read.")
    return(NULL)
  }
  data <- bind_rows(data_list)

}

# result_df <- process_EXO("data/Sensors/YSI EXO3/winter 2024 to spring 2025 deployment/Loch_EXO_20241024_20250430.csv")

