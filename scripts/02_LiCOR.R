source("scripts/00_libraries.R")

data_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data"        

output_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_cleaned_data"        

dir.create(output_path, showWarnings = FALSE)

discrepancy_report <- batch_compare_licor_entries(data_path, output_path)

View(discrepancy_report)

# if the discrepancy report comes back with no issues, retain the first file of each replicate. 

######FOR OTI ####

# the column names of our licor data do not match how the On Thin Ice project wants it report. Use the below code to adapt that file structure
# for data upload purposes (should be done somewhat regularly)

# library
library(tidyverse)

#redefine data path to point towards the export folder (licor_cleaned_data)
data_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data/"       

save_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_cleaned_data" 

files_with_duplicates = list.files(data_path)  |> 
  print()

files = files_with_duplicates[grepl("_1\\.csv$", files_with_duplicates)] |> 
  print()

for (i in 1:length(files)) {
  file_name = files[[i]]
  file_path = file.path(data_path, file_name)
  
  
  file_data = readr::read_csv(file_path) |> 
    drop_na(site)
  
  file_reformatted = file_data |> 
    dplyr::mutate(Record = 1:nrow(file_data), 
           `date_yyyy-mm-dd` = date, 
           `time_hhmmss` = time, 
           water_umolm2s1 = licorReading, 
           air_umoldm2s1 = NA_character_, 
           depth = licorDepth) |> 
    dplyr::select(Record, `date_yyyy-mm-dd`, `time_hhmmss`,
           water_umolm2s1, air_umoldm2s1, depth)
  
  # Create output name — prefix the original filename
  output_name <- paste0(file_name)
  output_path <- file.path(save_path, output_name)
  
  # Save uniquely named file
  readr::write_csv(file_reformatted, output_path)
  
  message("Saved: ", output_name)
}






























