source("functions/00_libraries.R")

#' Compile high-frequency data from HOBOs
#' 
#' @param filepath By default, the `main_dir` variable, which is defined in the global environment (usually at the beginning of each analysis script). Allows for flexibility for you to call the Sharepoint shortcut wherever it lives on your computer.
#' @return a dataframe with ~15 columns (filepath, lake_ID, date_time, timezone, sensor depth (from top or bottom), temperature, lux, and some extraneous other columns that are part of some HOBO output but not others)
#' @examples
#' main_dir <-  here("data/sensors/HOBO")
#' all_HOBO <- compile_HOBO_data(filepath = main_dir) 
#' # equivalent to compile_HOBO_data(main_dir) 
#' If you want to only include one particular lake in case too many files is bogging down your machine, simply adjust the directory path
#' loch_dir <- here("data/sensors/HOBO/LOC)
#' loch_HOBO <- compile_HOBO_data(loch_dir)



# HOBO compilation function  -------------------------------------------

compile_HOBO_data <- function(filepath = main_dir) {
  files <- list.files(main_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
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
      depth_m = map_chr(Parts, 2, .default = NA),
      depth_from = map_chr(Parts, 3, .default = NA),
      date_deployed = map_chr(Parts, 4, .default = NA),
      date_retrieved = map_chr(Parts, 5, .default = NA),
      logging_frequency = map_chr(Parts, 6, .default = NA) ,
      serial_num = map_chr(Parts, 7, .default = NA) %>% str_remove("\\.csv$") %>% str_remove("serial")
    ) %>%
    select(File, lake_ID, depth_m, depth_from, date_deployed, date_retrieved, logging_frequency, serial_num) %>%
    mutate(depth_m = suppressWarnings(as.numeric(str_remove(depth_m, "m"))))  
  
  # file <- files[[1]]
  
  # Read and clean files
  data_list <- map(files, function(file) {
    tryCatch({
      df <- vroom(file, delim = ",", show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
        mutate(File = file) %>%
        select(File, everything())  
      
      # Remove column named `#`, if it exists
      if ("#" %in% colnames(df)) {
        df <- df %>% select(-`#`)
      }
      
      # Standardize column names
      colnames(df) <- colnames(df) %>%
        str_replace_all("\\s+", "_")   # Replace spaces with underscores
      
      # Detect and extract timezone from column name
      time_col_name <- colnames(df)[str_detect(colnames(df), "Date-Time")]
      timezone <- ifelse(str_detect(time_col_name, "MDT"), "MDT",
                         ifelse(str_detect(time_col_name, "MST"), "MST", NA))
      
      # Rename date-time column
      df <- df %>%
        rename(date_time = all_of(time_col_name)) %>%
        mutate(timezone = timezone)  # Add timezone column
      
      # Rename columns only if they exist
      if (any(str_detect(colnames(df), "Temperature"))) {
        df <- df %>% rename_with(~ "temperature_C", matches("Temperature"))
      }
      if (any(str_detect(colnames(df), "Light"))) {
        df <- df %>% rename_with(~ "light_lux", matches("Light"))
      }
      if (any(str_detect(colnames(df), "Button_Down"))) {
        df <- df %>% rename_with(~ "button_down", matches("Button_Down"))
      }
      if (any(str_detect(colnames(df), "Button_Up"))) {
        df <- df %>% rename_with(~ "button_up", matches("Button_Up"))
      }
      if (any(str_detect(colnames(df), "Host_Connected"))) {
        df <- df %>% rename_with(~ "host_connected", matches("Host_Connected"))
      }
      if (any(str_detect(colnames(df), "End_of_File"))) {
        df <- df %>% rename_with(~ "end_of_file", matches("End_of_File"))
      }
      
      
      return(df)
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
  
  data <- bind_rows(data_list) %>%
    left_join(metadata, by = "File") %>%
    mutate(
      date_time = parse_date_time(date_time, orders = c("mdy_HM", "mdy_HMS")),  # Auto-detects format                                   # 1. parse, still naïve
      date_time = case_when(                                           # 2. pin the real zone
        timezone == "MDT" ~ force_tz(date_time, "America/Denver"),     #    recorded in MDT (UTC‑6)
        timezone == "MST" ~ force_tz(date_time, "Etc/GMT+7"),          #    recorded in fixed MST (UTC‑7)
        TRUE ~ date_time                                               #    safety net
      ),
      date_time = with_tz(date_time, "America/Denver"),                 # 3. show everything in local Denver time
      depth_from_top = case_when(lake_ID == "SKY" & depth_from == "BOT" ~ 7 - depth_m,
                                 lake_ID == "SKY" & depth_from == "TOP" ~ depth_m,
                                 lake_ID == "LOC" & depth_from == "BOT" ~ 5 - depth_m,
                                 lake_ID == "LOC" & depth_from == "TOP" ~ depth_m,
                                 lake_ID == "FER" & depth_from == "BOT" ~ 5.5 - depth_m,
                                 lake_ID == "FER" & depth_from == "TOP" ~ depth_m),
      depth_from_bottom = ifelse(depth_from == "BOT", depth_m, NA_real_)
    ) %>%
    select(File, lake_ID, date_time, timezone, depth_from_top, depth_from_bottom, everything()) %>%
    select(-depth_m, -depth_from) %>%  #delete metadata columns to reduce confusion
    select(-c(button_down:Stopped)) #delete HOBOs columns unless we decide we need them later
  
  return(data)
  
  
}


###################
# Exploring
###################



main_dir <-  here("data/sensors/HOBO")
all_HOBO <- compile_HOBO_data(filepath = main_dir)
str(all_HOBO)
length(unique(all_HOBO$date_retrieved))

# Plot all Loch data 
pretty <- all_HOBO %>%
  filter(date_deployed=="20241025")
all_HOBO %>%
  filter(date_deployed=="20241025") %>%
  # filter(date_time >= "2024-10-26" & date_time < "2025-05-21") %>%
  filter(date_time >= "2024-10-26" & date_time < "2024-11-30") %>%
  ggplot(aes(x=date_time, y=temperature_C, color=factor(depth_from_top)))+
  geom_point(alpha=0.5)
