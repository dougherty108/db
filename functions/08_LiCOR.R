source("scripts/00_libraries.R")

# placeholder script for LiCOR profiles. I'll see if I have any sample data
  # that I can use to write a skeleton script from -AGK

# source functions

# compile LiCOR profiles for GL4

harmonize_double_entry <- function(file1, file2, flag_mismatch = TRUE, flag_prefix = "MISMATCH:") {
  # Read both CSVs
  df1 <- read_csv(file1)
  df2 <- read_csv(file2)
  
  # Check same dimensions
  if (!all(dim(df1) == dim(df2))) {
    stop("CSV files have different dimensions.")
  }
  
  # Create a harmonized data frame
  harmonized <- df1  # initialize with df1 values
  
  # Iterate over each cell to compare
  for (i in seq_len(nrow(df1))) {
    for (j in seq_len(ncol(df1))) {
      val1 <- as.character(df1[i, j])
      val2 <- as.character(df2[i, j])
      
      if (val1 != val2) {
        if (flag_mismatch) {
          harmonized[i, j] <- paste0(flag_prefix, val1, " | ", val2)
        } else {
          harmonized[i, j] <- NA  # or choose val1/val2 if you want to prefer one
        }
      }
    }
  }
  
  return(harmonized)
}

# test, load two files

loc_1 = read_csv("~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data/loc_20250622_1.csv")
loc_2 = read_csv("~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data/loc_20250622_2.csv")


result <- harmonize_double_entry("~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data/loc_20250522_1.csv", 
                                 "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data/loc_20250522_2.csv")

# View mismatches
View(result)



