source("scripts/00_libraries.R")

# placeholder script for LiCOR profiles. I'll see if I have any sample data
  # that I can use to write a skeleton script from -AGK

# source functions

# compile LiCOR profiles for GL4

licor_double_entry <- function(file1, file2, data_path, flag_mismatch = TRUE, flag_prefix = "MISMATCH:") {
  # Read both CSVs
  df1 <- read.csv(file.path(data_path, file1), stringsAsFactors = FALSE)
  df2 <- read.csv(file.path(data_path, file2), stringsAsFactors = FALSE)
  
  # Check same dimensions
  if (!all(dim(df1) == dim(df2))) {
    stop("CSV files have different dimensions.")
  }
  
  # Initialize harmonized and mismatch storage
  harmonized <- df1  # start with values from df1
  mismatches <- data.frame(row = integer(), column = character(),
                           val1 = character(), val2 = character(), stringsAsFactors = FALSE)
  
  # Iterate over each cell to compare
  for (i in seq_len(nrow(df1))) {
    for (j in seq_len(ncol(df1))) {
      val1 <- as.character(df1[i, j])
      val2 <- as.character(df2[i, j])
      
      # Compare, taking care with NAs
      if (is.na(val1) != is.na(val2) || (!is.na(val1) && !is.na(val2) && val1 != val2)) {
        colname <- colnames(df1)[j]
        mismatches <- rbind(mismatches, data.frame(
          row = i,
          column = colname,
          val1 = val1,
          val2 = val2,
          stringsAsFactors = FALSE
        ))
        
        if (flag_mismatch) {
          harmonized[i, j] <- paste0(flag_prefix, val1, " | ", val2)
        } else {
          harmonized[i, j] <- NA
        }
      }
    }
  }
  
  return(list(data = harmonized, mismatches = mismatches))
}


# test, load two files
data_path = "~/OneDrive-SharedLibraries-UCB-O365/Mountain limnology lab - Data/00_Data Entry/LiCOR Data Entry/licor_entered_data"

file1 = "gl4_20250422_1.csv"
file2 = "gl4_20250422_2.csv"

result <- licor_double_entry(file1, file2, data_path)

# View mismatches
View(result)

###################################

parse_filename <- function(filename) {
  # Expect: something like "gl4_20250415_1.csv"
  parts <- strsplit(filename, "_|\\.")[[1]]
  if (length(parts) < 3) return(NULL)
  
  list(
    prefix = parts[1],
    date = parts[2],
    rep = parts[3]
  )
}

batch_compare_licor_entries <- function(data_path, output_folder = NULL) {
  # List all files ending in _1.csv or _2.csv
  all_files <- list.files(data_path, pattern = "_[12]\\.csv$", full.names = FALSE)
  
  # Helper function to parse filename parts
  parse_filename <- function(filename) {
    # Expected format: prefix_date_rep.csv (e.g., gl4_20250415_1.csv)
    parts <- strsplit(filename, "_|\\.")[[1]]
    if (length(parts) < 3) return(NULL)
    
    list(
      filename = filename,
      prefix = parts[1],
      date = parts[2],
      rep = parts[3]
    )
  }
  
  # Parse all filenames
  parsed <- lapply(all_files, parse_filename)
  parsed <- do.call(rbind, lapply(parsed, as.data.frame, stringsAsFactors = FALSE))
  
  # Group by prefix + date, and look for both replicates
  library(dplyr)
  file_pairs <- parsed %>%
    group_by(prefix, date) %>%
    filter(all(c("1", "2") %in% rep)) %>%  # only keep those with both replicates
    arrange(rep) %>%
    summarise(
      file1 = filename[rep == "1"],
      file2 = filename[rep == "2"],
      .groups = "drop"
    )
  
  if (nrow(file_pairs) == 0) {
    message("No complete replicate pairs found.")
    return(NULL)
  }
  
  # Storage for all mismatches across all file pairs
  all_mismatches <- data.frame()
  
  # Compare each file pair
  for (i in seq_len(nrow(file_pairs))) {
    file1 <- file_pairs$file1[i]
    file2 <- file_pairs$file2[i]
    id <- paste(file_pairs$prefix[i], file_pairs$date[i], sep = "_")
    
    message("Comparing: ", file1, " and ", file2)
    
    result <- licor_double_entry(file1, file2, data_path)
    
    # Save harmonized data if output folder provided
    if (!is.null(output_folder)) {
      dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
      out_file <- file.path(output_folder, paste0(id, "_harmonized.csv"))
      write.csv(result$data, out_file, row.names = FALSE)
    }
    
    # If mismatches found, record them
    if (!is.null(result$mismatches) && nrow(result$mismatches) > 0) {
      result$mismatches$file_id <- id  # add identifier column
      all_mismatches <- rbind(all_mismatches, result$mismatches)
    }
  }
  
  # Write full mismatch report
  if (!is.null(output_folder) && nrow(all_mismatches) > 0) {
    report_file <- file.path(output_folder, "licor_discrepancy_report.csv")
    write.csv(all_mismatches, report_file, row.names = FALSE)
    message("Discrepancy report saved to: ", report_file)
  }
  
  # Return the full mismatch table (invisible if used in pipeline)
  return(all_mismatches)
}





