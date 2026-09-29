# ----------------------------------------------------------------------------------
# This script was written by Adeline G. Kelly and simply takes the YSI Pro DSS 
# file and converts it into a CSV and make the headings all pretty. This 
# function is meant for looking at lake profiles. See script 02_ysi_point.R 
# for a point measurement function.
# The KOR software started exporting the CSVs in a different format end of 2025
# so Bella updated the function to be more flexible with recognizing header names
# on June 1, 2026
# ----------------------------------------------------------------------------------

process_ysi <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_]")[[1]]
  
  # Read in file
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  
  # Fix encoding and special characters in column names
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- toupper(colnames(data))
  colnames(data) <- gsub("<B5>", "µ", colnames(data))
  
  # Define rename map (old = new)
  rename_map <- c(
    "DATE (MM/DD/YYYY)"  = "date",
    "DATE (M/D/YYYY)"    = "date",
    "TIME (HH:MM:SS)"    = "time",
    "TIME (H:MM:SS TT)"  = "time",
    "CHLOROPHYLL RFU"    = "chla_RFU",
    "COND ΜS/CM"         = "cond_uScm",
    "COND ÎŒS/CM"        = "cond_uScm",
    "DEPTH M"            = "depth_m",
    "ODO % SAT"          = "do_percent",
    "ODO MG/L"           = "do_mgL",
    "ORP MV"             = "orp_mV",
    "SPCOND ΜS/CM"       = "cond_spec_uScm",
    "SPCOND ÎŒS/CM"      = "cond_spec_uScm",
    "TAL PC RFU"         = "phycoC_RFU",
    "PH"                 = "pH",
    "TEMP °C"            = "temp_C",
    "BAROMETER MMHG"     = "barometer_mmHg"
  )
  
  # Keep only names that exist in the data
  existing_rename_map <- rename_map[names(rename_map) %in% colnames(data)]
  
  # Safely rename columns
  data <- data %>% rename(!!!setNames(names(existing_rename_map), existing_rename_map))
  
  # Merge date and time, parsing both 12hr and 24hr formats
  if (all(c("date", "time") %in% colnames(data))) {
    data <- data %>%
      mutate(date_time = parse_date_time(
        paste(date, time),
        orders = c("m/d/Y H:M:S", "m/d/Y I:M:S p"),  # 24hr then 12hr AM/PM
        quiet = TRUE
      ))
  } else {
    data$date_time <- NA
  }
  
  # Desired columns (only keep those that exist)
  desired_cols <- c("date_time", "chla_RFU", "cond_uScm", "depth_m", "do_percent", 
                    "do_mgL", "orp_mV", "cond_spec_uScm", "phycoC_RFU", 
                    "pH", "temp_C", "barometer_mmHg")
  keep_cols <- intersect(desired_cols, colnames(data))
  
  # Build final dataframe
  data <- data %>%
    select(all_of(c("date_time", keep_cols))) %>%
    mutate(lake = file_info[1],
           site = file_info[2]) %>%
    relocate(lake, .before = everything()) %>%
    relocate(site, .after = lake) %>%
    relocate(depth_m, .after = date_time) %>%
    # mutate(date_time = suppressWarnings(mdy_hms(date_time)),
    # date = as.Date(date_time)) %>%
    pivot_longer(cols = any_of(setdiff(keep_cols, c("date_time", "depth_m"))), 
                 names_to = "parameter")
  
  return(data)
}

# Write plotting function 
Round_Plot_YSI_FUNC <- function(ysi_profile, round_to_nearest) {
  
  # Title pieces, built from the raw input
  plot_lake <- paste(unique(ysi_profile$lake), collapse = ", ")
  plot_date <- format(unique(as.Date(ysi_profile$date_time, tz = "America/Denver")),
                      "%B %d, %Y")
  plot_date <- paste(plot_date, collapse = " / ")
  
  ysi_profile %>%
    mutate(depth_m = round(depth_m / round_to_nearest) * round_to_nearest) %>%
    group_by(depth_m, parameter, lake) %>%
    summarize(value = median(value, na.rm = TRUE), .groups = "drop") %>%
    filter(!parameter %in% c("barometer_mmHg", "cond_spec_uScm")) %>%
    ggplot(aes(x = value, y = depth_m, color = parameter)) +
    geom_point() +
    scale_y_reverse() +
    facet_wrap(parameter ~ ., scales = "free_x", nrow = 2) +
    labs(title = paste(plot_lake, "-", plot_date),
         x = "Value", y = "Depth (m)")
}

# Write a function that rounds, summarizes by depth, and pivots the data to wide format 
OTI_YSI_FUNC <- function(ysi_profile, round_to_nearest){

      # Round to the nearest depth (based on what you decided looking at the plots ) 
      ysi_profile_rounded <- ysi_profile %>%
        mutate(depth_m=round(depth_m/ round_to_nearest )* round_to_nearest ) 

      # Save the date time that the profile was collected as date 
      ysi_profile_rounded$date <- ysi_profile_rounded$date_time[1] # funky because we want to keep the date-time that the profile was taken but we don't want to summarize by time bc it would replicate for each second or minute and some of our profiles cover a lot of time 

      # Summarize: take median parameter value for each unique combination of lake, site, date, depth
      ysi_profile_summarized <- ysi_profile_rounded %>% #round to the nearest 0.5
        group_by(lake, site, date, depth_m, parameter) %>% # gather everythinf into groups correspond to a unique combination of lake, date, depth
        summarise(value = median(value, na.rm = TRUE), .groups = "drop") # take the median of each group 

      # Pivot the resulting table from long format to wide format 
      ysi_wide <- ysi_profile_summarized %>%
        select(lake, site, date, depth_m, parameter, value) %>%  # keep relevant columns
        pivot_wider(
          names_from = parameter,   # each unique parameter becomes its own column
          values_from = value       # fill those columns with the 'value' data
        )
       
      # Format columns and column names 

          # some columns just need to be renamed 
          names(ysi_wide)[names(ysi_wide) == "lake"] <- "lakeID" 
          names(ysi_wide)[names(ysi_wide) == "temp_C"] <- "temp_degC" 
          names(ysi_wide)[names(ysi_wide) == "do_mgL"] <- "doConcentration_mgpL" 
          names(ysi_wide)[names(ysi_wide) == "do_percent"] <- "doSaturation_percent"
          names(ysi_wide)[names(ysi_wide) == "cond_spec_uScm"] <- "specificConductivity_uSpcm" 

          # Format dates to be compatable 
          ysi_wide$date_yyyy.mm.dd <- as.Date(ysi_wide$date)
          ysi_wide$time_hhmmss <- format(ysi_wide$date, "%H:%M:%S")

          # Convert the units of barometric pressure to tbe same as the rest of the OTI Team 
          ysi_wide$waterPressure_barA <- ysi_wide$barometer_mmHg * 0.0013322 # we measure barometric pressure as barometer_mmHg, for "water pressure" (under water rather than in air handheld) Dave wanrs barA as the units 
          
          # Some parameters we don't collect on our instrument so give them a column with explicit NAs 
          ysi_wide$turbidity_FNU <- NA # explicit column of NAs for data that we do not have 
          ysi_wide$salinity_psu <- NA # explicit column of NAs for data that we do not have
          ysi_wide$tds_mgpL <- NA # explicit column of NAs for data that we do not have
          ysi_wide$barometerAirHandheld_mbars <- NA # explicit column of NAs for data that we do not have 

          # Set lat long and altitude based on lake 
          ysi_wide$latitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_lat, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_lat, NA)) 
          ysi_wide$longitude <- ifelse(ysi_wide$lakeID == "GL4", gl4_long, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_long, NA))
          ysi_wide$altitude_m <- ifelse(ysi_wide$lakeID == "GL4", gl4_alt, 
                                  ifelse(ysi_wide$lakeID == "LOC", loc_alt, NA))

          # We have some timepoints for the loch where we have CHLA and PHYC but other time points when we don't. Set it up so that if we have data it populates and if not the column gets explocot NAs 
          ysi_wide <- ysi_wide %>% mutate(chlorophyll_RFU  = if ("chla_RFU" %in% names(.)) chla_RFU else NA) # take ysi_wide and make a new column called "chlorophyll_RFU" (what Dave wants this called), if the data frame includes a column named "chla_RFU" (what we name that column), then use the data from that column. If there is no column with that name (if we don't have that data) then fill the column with NAs 
          ysi_wide <- ysi_wide %>% mutate(phycocyaninBGA_RFU  = if ("phycoC_RFUU" %in% names(.)) phycoC_RFU else NA)
          ysi_wide <- ysi_wide %>% mutate(pH  = if ("pH" %in% names(.)) pH else NA) # also for some reason some timepoints with no pH and no orp
          ysi_wide <- ysi_wide %>% mutate(orp_mV = if ("orp_mV" %in% names(.)) orp_mV else NA)


      # Put all together into one nice formatted dataframe 
      ysi_clean <- subset(ysi_wide, select = c("lakeID" , "date_yyyy.mm.dd", "time_hhmmss", "depth_m", "temp_degC", "doConcentration_mgpL",
                                    "doSaturation_percent", "chlorophyll_RFU", "phycocyaninBGA_RFU", "turbidity_FNU", "pH", "orp_mV", 
                                    "specificConductivity_uSpcm", "salinity_psu", "tds_mgpL", "waterPressure_barA", "latitude",
                                      "longitude", "altitude_m", "barometerAirHandheld_mbars" ))

      return(ysi_clean)
    }


# Example below for profile measurement
# result_df <- process_ysi("Data/On Thin Ice/01_YSI/LOC/raw/Loch_Zmax_20250415.csv")
result_df <- process_ysi("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/GL4/raw/GL4_Zmax_20251216.csv")
data <- read.csv("~/OneDrive - UCB-O365/Research/Data/R/sensor_db/data/Sensors/YSI Pro DSS/GL4/raw/GL4_Zmax_20251216.csv",
                 sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
