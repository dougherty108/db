source("functions/00_libraries.R")

#' Compile high-frequency data from HOBOs
#' 
#' @param filepath By default, the `filepath` variable, which is defined in the global environment (usually at the beginning of each analysis script). Allows for flexibility for you to call the Sharepoint shortcut wherever it lives on your computer.
#' @return a dataframe with ~15 columns (filepath, lake_ID, date_time, timezone, sensor depth (from top or bottom), temperature, lux, and some extraneous other columns that are part of some HOBO output but not others)
#' @examples
#' filepath <-  here("data/sensors/HOBO")
#' all_HOBO <- compile_HOBO_data(filepath = filepath) 
#' # equivalent to compile_HOBO_data(filepath) 
#' If you want to only include one particular lake in case too many files is bogging down your machine, simply adjust the directory path
#' loch_dir <- here("data/sensors/HOBO/LOC")
#' loch_HOBO <- compile_HOBO_data(loch_dir)



# HOBO compilation function  -------------------------------------------

compile_HOBO_data <- function(filepath) {
  files <- list.files(filepath, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
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
                                 lake_ID == "FER" & depth_from == "TOP" ~ depth_m,
                                 lake_ID == "TUC" & depth_from == "BOT" ~ 16.6 - depth_m,
                                 lake_ID == "UFM" & depth_from == "BOT" ~ 9 - depth_m,
                                 lake_ID == "LFM" & depth_from == "BOT" ~ 6.4 - depth_m),
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



# filepath <-  here("data/sensors/HOBO")
# all_HOBO <- compile_HOBO_data(filepath = filepath)
# str(all_HOBO)
# length(unique(all_HOBO$date_retrieved))

# # Plot all Loch data 
# pretty <- all_HOBO %>%
#   filter(date_deployed=="20241025")
# all_HOBO %>%
#   filter(date_deployed=="20241025") %>%
#   # filter(date_time >= "2024-10-26" & date_time < "2025-05-21") %>%
#   filter(date_time >= "2024-10-26" & date_time < "2024-11-30") %>%
#   ggplot(aes(x=date_time, y=temperature_C, color=factor(depth_from_top)))+
#   geom_point(alpha=0.5)


#########################
# Katie's process hobo file function 
#   GOAL: format hobo file column names and creates columns based on the file name
#   input: file path to a single csv file of hobo data 
#   ouput: formatted df of hobo data 

    process_hobo_file <- function(hobo_file_path){
        # Extract file name and save file info 
            # Extract file name so that we can use that information and put it as columns 
            file_name <- tools::file_path_sans_ext(basename(hobo_file_path)) 
                  # basename() -> removes everything before the last / (so the path to the file)
                  # file_path_sans_ext() --> removes .csv
              
              # Extract information from folder name (delimited by underscores)
              file_info <- strsplit(file_name, "_")[[1]] # not this says take that folder name and split it into seperate objects, based on the "_"
        
        # add a note to see progress and where any issue is occuring 
        message("Reading: ", basename(hobo_file_path))        
      
        # Read the data from the file
        data <- read.csv(hobo_file_path, sep = ",")

        # Format Data 
 
        data <- data %>%
            janitor::clean_names() %>% # this is a package that turns everything to lower case and snake case 
            rename_with(~"date_time", matches("date.*time")) %>% #then this says anything with date and time in it call it date_time
            rename_with(~"temp", matches("temp")) %>%
            rename_with(~"lux", matches("light")) %>%
            subset(select = c("date_time", "temp", "lux")) %>% # now grab only the columns that you want 
            mutate(  # add the information from the file name as columns in the data 
              siteID = file_info[1], # needs to be siteID to match the SQL database 
              depth = file_info[2], 
              depth_from = file_info[3], 
              logging_interval = file_info[6],
              sensor_num = substring(file_info[7], 7, 20), 
              location = ifelse(length(file_info) >= 8, file_info[4], "zmax") #this is adding a little flexibility for when we have shore sites and the file path indicates which site they are at
                    # this last location line of code says "check the length of the folder info object, if it contains 4 or more objects then use the forth to designate the location column, otherwise, call it zmax"
            ) %>%
          mutate(siteID = tolower(siteID), # change all the site ids to lower case to match other data streams 
                date_time = parse_date_time(date_time, orders = c("mdy_HM", "mdy_HMS")),  # Auto-detects format  
            ) 

      # Try and format the datetime for the hobos 
      data$date_time <- as.POSIXct(data$date_time)
      
       # return the formatted data file 
        return(data)
    }