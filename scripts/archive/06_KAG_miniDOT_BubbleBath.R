# miniDOT Bubble Bath Calibation Check ----
# KAG 20250820


# 00 Set up R Environment 
source("functions/00_libraries.R")
source("functions/00_helper_functions.R")
source("functions/03_miniDOT.R")


# 01 Load in and format miniDOT data ----

    # Define the path to the main directory where the data you are working with is stored 
    main_dir <- here("/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/miniDOT/20250606_miniDot_BubbleBath")
    
    # Get full paths of all files in "20250606_miniDot_BubbleBath" folder
    file_list <- list.files(main_dir, full.names = TRUE)
    
    # Define a function to process each file
    
        # Dummy data 
        # file_path <- "/Users/kaga3666/Library/CloudStorage/OneDrive-UCB-O365/Graduate_School/04_Mountain_Limno_Lab/01_Data/Sensor_Data/miniDOT/20250606_miniDot_BubbleBath/2025-06-06_037344.txt"
        
    
    process_file <- function(file_path) {
          
          # Extract the file name because this has the sensor number in it 
          file_name <- basename(file_path) #Save only the file name 
          sensor_number <- sub(".*_(.*)\\.txt", "\\1", file_name) # extract the sensor number from the file name, sensor number is the characters between the "_" and the "."
          
          # Read the data from the file
          data <- read.table(file_path, sep = ",", skip = 2, header = TRUE)
          
          # Add new columns based on the folder name
          data <- data %>%
            select(`Time..sec.`,`T..deg.C.`,`DO..mg.l.`) %>% # Select and rename columns that have funky names 
            dplyr::rename(date_time = `Time..sec.`,
                          temp = `T..deg.C.`,
                          do_obs = `DO..mg.l.`) %>%
            mutate(local_tz = "Mountain", # Format time by including a column for timezone 
                   sensor_number = sensor_number, # Save the sensor number as its own column )
                   daylight_savings = "Yes",
                   date_time = as_datetime(as.numeric(date_time), tz = "UTC"), 
                   # Convert from Unix time (in UTC)
                   date_time = with_tz(date_time, tz = "America/Denver") 
            )
          
          return(data)
        }
        
        # Apply the processing function across all files 
        processed_list <- lapply(file_list, process_file)
        head(processed_list[[3]])
        
    # Put all data frames together 
        miniDot_data <- do.call(rbind, processed_list)
        
    # Calculate oxygen saturation based on temp, do mg/L and elevation of boulder 
        miniDot_data$do_sat <- miniDot_data$do_obs/(oxySol(miniDot_data$temp, 0, 0.82)) *100 # equation pulled from IAO function 03_miniDOT functions script, calculating what the mg/L would be at 100% saturation then comparing that to observed, changed the atm to 0.82 for boulder elevation atmospheric pressure 
        head(miniDot_data)
    
    # Trim data to only include time when in the bubble bath 
        miniDot_data_trimmed <- miniDot_data[miniDot_data$date_time >= as.POSIXct("2025-06-06 10:30:00") , ] # Trim to only after the sensors were placed in the bath 
        miniDot_data_trimmed <- miniDot_data_trimmed[miniDot_data_trimmed$date_time <= as.POSIXct("2025-06-06 16:15:00") , ] # Trim to only before sensors where removed from bath 

# 02 Plot DO over time ----
    ggplot(miniDot_data_trimmed, aes(x = date_time, y = do_sat)) +
      geom_line(aes(color = sensor_number)) +   # connect points with a line
      geom_point(aes(color = sensor_number)) +  # add points
      geom_vline(xintercept = as.POSIXct("2025-06-06 13:20:00"), 
                 linetype = "dashed", color = "black") +
      labs(
        x = "Date & Time",
        y = "DO Saturation (%)",
        title = paste("DO Saturation for Sensor")
      ) +
      theme_minimal()
    
    
   
    
    
    
    
    