source("scripts/00_libraries.R")

# placeholder script for LiCOR profiles. I'll see if I have any sample data
  # that I can use to write a skeleton script from -AGK

# source functions

# compile LiCOR profiles for GL4

# function to run the double entry checks and to make sure columns are correct
LICOR_double_entry_check <- function(log_path, df1, df2, lake) {
  check_double_entry(log_path, df1, df2)
  check_column_names(log_path, df1)
  write_to_log(log_path, "\n")
  
  already_checked_path <- file.path("Data", "log_files", "double_entry", "double_entry_files_already_checked.txt")
  write_to_log(already_checked_path, lake)
}

main <- function() {
  # create a new log file to write to
  log_path <- file.path("Data", "log_files", "double_entry", "double_entry.txt")
  
  # Grabs all of the csvs
  lake_pairs <- pull_in_all_files(log_path)
  
  # loops through pairs of lakes.
  for (lake in names(lake_pairs)) {
    print(lake)
    
    write_to_log(log_path, glue("-------- Current lake: {lake} --------"))
    
    # From lake pair, grab the df's corresponding to the current lake.
    # Pair will then contain two dfs. We can pull them out of pair and 
    # assign them to new names.
    pair <- lake_pairs[[lake]] 
    df1 <- pair$df1
    df2 <- pair$df2
    
    df1 <- df1[, !(names(df1) %in% drops)]
    df2 <- df2[, !(names(df2) %in% drops)]
    
    # mark as checked
    #TODO: make this only so if there is nothing to fix in the data
    double_entry_check(log_path, df1, df2, lake)
  }  
  
  # takes a minute for it to load entirely, force user to wait.
  cat("\014")
  cat("Please wait while log file is generated!")
  Sys.sleep(2) 
  cat("\014")
  cat("Please see Data/log_files/double_entry/double_entry.txt to view the output.\n")
  cat("If file is missing lakes, close and re-open and the rest of the data will populate.\n")
  cat("To view running list of previously tested lakes, navigate to:\n")
  cat("Data/log_files/double_entry/double_entry_files_already_checked.txt\n")
}

main()

process_LICOR <- function(file_path) {
  # Extract information from file name
  file_name <- path_file(file_path)
  file_info <- strsplit(file_name, "[_]")[[1]]
  
  # Read in file
  data <- read.csv(file_path, sep = ",", header = TRUE, skip = 18, skipNul = TRUE, check.names = FALSE)
  
  # Fix encoding and special characters in column names
  Encoding(colnames(data)) <- "latin1"
  colnames(data) <- gsub("<b5>", "µ", colnames(data))
  
  # Merge date and time if both exist
  if (all(c("date", "time") %in% colnames(data))) {
    data <- data %>%
      mutate(date_time = paste(date, time))
  } else {
    data$date_time <- NA
  }
  
  
  # Build final dataframe
  data <- data %>%
    #select(all_of(c("date_time", keep_cols))) %>%
    mutate(lake = file_info[1],
           site = file_info[2]) %>%
    relocate(lake, .before = everything()) %>%
    relocate(site, .after = lake) %>%
    relocate(depth_m, .after = date_time) %>%
    mutate(date_time = suppressWarnings(mdy_hms(date_time)),
           date = as.Date(date_time)) %>%
    pivot_longer(cols = any_of(setdiff(keep_cols, c("date_time", "depth_m"))), 
                 names_to = "parameter")
  
  return(data)
}



























