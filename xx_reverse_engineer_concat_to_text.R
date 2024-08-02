source("scripts/00_libraries.R")


#Sky Pond first
sky_concat <- bind_rows(read.table("data/Historical Loch Vale/miniDOT/concat/Sky/2016_17_SkyLH/Sky_6.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5") %>%
    mutate(date_time = as_datetime(`date_time`)),
  read.table("data/Historical Loch Vale/miniDOT/concat/Sky/2016_17_SkyLS/Sky_0.5m_16-17_all.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)),
  read.table("data/Historical Loch Vale/miniDOT/concat/Sky/2017_18_SkyLH/sky_hypo_Oct17-Sept18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "6.5") %>%
    mutate(date_time = as_datetime(`date_time`)),
  read.table("data/Historical Loch Vale/miniDOT/concat/Sky/2017_18_SkyLS/Sky_surface_Oct17-June18.TXT", sep = ",", header = FALSE, skip = 9, strip.white = TRUE) %>%
    select(3, 5:7) %>%
    dplyr::rename(date_time = 1, temp = 2, do_obs = 3, do_sat = 4) %>%
    mutate(lake_id = "sky", local_tz = "Mountain", daylight_savings = "Yes", depth = "0.5") %>%
    mutate(date_time = as_datetime(`date_time`)))

sky_new <- sky_concat %>%
  mutate(depth_from="T",
         date=date(date_time)) %>%
  select(-local_tz, -daylight_savings, -do_sat)
  # rename(`Time..sec.`date_time,`T..deg.C.`=temp,`DO..mg.l.`=do_obs)




# Define the base directory where the folders will be created
base_dir <- "data/LVWS/05_miniDOT/concat_to_daily"

# Get unique combinations of 'lake_id', 'depth', and 'depth_from'
unique_combinations <- sky_new %>%
  select(lake_id, depth, depth_from) %>%
  distinct()

# Loop through each combination
for (combination in seq_len(nrow(unique_combinations))) {
  
  # Extract current combination
  current_combination <- unique_combinations[combination, ]
  lake_id <- current_combination$lake_id
  depth <- current_combination$depth
  depth_from <- current_combination$depth_from
  
  # Create a directory for the current combination
  dir_path <- file.path(base_dir, lake_id, depth, depth_from)
  dir_create(dir_path) # Ensure the directory is created
  
  # Filter data for the current combination
  filtered_data <- sky_new %>%
    filter(lake_id == lake_id, depth == depth, depth_from == depth_from) %>%
    select(date_time, temp, do_obs)
  
  # Get unique dates for the current combination
  unique_dates <- unique(filtered_data$date)
  
  # Write a file for each unique date
  for (date in unique_dates) {
    # Filter data for the current date
    date_data <- filtered_data %>%
      filter(date == date)
    
    # Create a filename that includes the date and depth
    file_name <- paste0("data_", date, "_", depth, ".txt")
    file_path <- file.path(dir_path, file_name)
    
    # Write the blank lines and data to the file
    writeLines(c("", ""), file_path)  # Write two blank lines
    write.table(date_data, file = file_path, sep = ",", row.names = FALSE, quote = FALSE, append = TRUE)
    
  }
}







#TEST FIRST

# Sample data frame
base_dir <- "data/LVWS/05_miniDOT/concat_to_daily"

df <- data.frame(
  date_time = Sys.time() + 1:10,
  temp = rnorm(10),
  do_obs = rnorm(10),
  lake_id = rep(c("lake1", "lake2"), each = 5),
  depth = rep(c("shallow", "deep"), 5),
  depth_from = rep(c("surface", "bottom"), 5),
  date = as.Date("2024-08-01") + 0:4
)

# Function to export files
export_files <- function(df) {
  
  # Ensure the 'date' column is in Date format if it's not already
  df$date <- as.Date(df$date)
  
  # Get unique combinations of 'lake_id', 'date', 'depth', 'depth_from'
  unique_combinations <- df %>%
    select(lake_id, date, depth, depth_from) %>%
    distinct()
  
  # Loop over each unique combination
  for (i in seq_len(nrow(unique_combinations))) {
    # Extract current combination
    combo <- unique_combinations[i, ]
    
    lake_id <- combo$lake_id
    date <- combo$date
    depth <- combo$depth
    depth_from <- combo$depth_from
    
    # Filter the dataframe for this specific combination
    subset_df <- df %>%
      filter(lake_id == lake_id & date == date & depth == depth & depth_from == depth_from)
    
    # Create folder name
    folder_name <- paste(lake_id, depth, depth_from, sep = "_")
    
    # Create directory if it doesn't exist
    if (!dir.exists(folder_name)) {
      dir.create(folder_name)
    }
    
    # Create file name for each date in the subset
    dates <- unique(subset_df$date)
    
    for (date in dates) {
      # Filter the dataframe for the current date
      date_subset_df <- subset_df %>% filter(date == date)
      
      # Create file name
      file_name <- paste(date, lake_id, depth, sep = "_")
      file_path <- file.path(folder_name, paste0(file_name, ".txt"))
      
      # Write the subset to a text file
      write.table(date_subset_df, file = file_path, sep = ",", row.names = FALSE, quote = FALSE)
      
      message("Exported: ", file_path)
    }
  }
}

# Call the function with your dataframe
export_files(sky_new)
