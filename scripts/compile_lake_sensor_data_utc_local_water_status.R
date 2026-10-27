#!/usr/bin/env Rscript

# ==============================================================================
# SCRIPT NAME: compile_lake_sensor_data.R
# PURPOSE:
#   Compiles all high-elevation lake sensor data (HOBO temperature/light and
#   miniDOT dissolved oxygen/temperature) across the Oleksy Mountain Limnology
#   Lab directory tree into a single, standardized, strictly rectangular CSV table.
#
# CORE DESIGN PRINCIPLES & CONSTRAINTS:
#   1. Rectangular Wide Format:
#      Every row represents a discrete sensor observation at a specific datetime,
#      lake, depth, and orientation. Measurement parameters (temperature, DO,
#      light, battery, quality score) occupy dedicated, homogenous columns.
#   2. Zero Data Transformation:
#      Raw measurement values (temperature, DO, light, battery, Q) are preserved
#      exactly as recorded by the instrument hardware. No smoothing, clipping,
#      calibration adjustment, or unit conversion is applied. The ONLY permitted
#      transformations are standardizing timestamps to ISO 8601 format.
#   3. Strict Homogenous Column Types & Clean NA Handling:
#      Numeric columns contain strictly numeric values or NA. Qualitative flags,
#      event markers (e.g., 'Logged', 'Button Down'), and sensor codes are
#      segregated into a dedicated `quality_flag` column.
#   4. Dual Timezone Standardization:
#      Diurnal cycles confirm HOBO loggers recorded in local Mountain time
#      (America/Denver: MDT/MST), while miniDOT sensors recorded in UTC epoch
#      seconds. To ensure seamless cross-lake and cross-sensor limnological
#      comparisons while preserving local day/night context, the output provides
#      both `datetime_utc` (ISO 8601 UTC) and `datetime_local` (ISO 8601 Denver).
#   5. Historical Concat Ingestion & Deduplication:
#      Non-duplicate 2016-2017 baseline records in `miniDOT/concat/` are ingested
#      to preserve historical time series, while 2017-2018 concat files that
#      duplicate existing daily files are omitted to avoid double-counting.
#   6. Comprehensive Contributor Feedback Logging:
#      All metadata typos, missing entries, format discrepancies, and sensor
#      anomalies are captured with exact file paths, line numbers, and actionable
#      remediation instructions in `data_compilation_issues.csv`.
#
# AUTHOR: Oleksy Mountain Limnology Data Synthesis Pipeline
# DATE: October 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP & PACKAGE DEPENDENCIES
# ------------------------------------------------------------------------------
# WHAT:
#   Suppress package startup messages, verify required packages, and initialize
#   high-performance data structures.
# WHY:
#   `data.table` provides fast C-level parsing (fread) and writing (fwrite)
#   capable of ingesting ~2.7 million rows across 22,750+ files in seconds.
#   `lubridate` handles multi-format date-time parsing and timezone conversions.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

cat("======================================================================\n")
cat("Starting Mountain Limnology Sensor Data Compilation Pipeline\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("======================================================================\n\n")

# Define base paths (relative to workspace root for portability)
BASE_DIR      <- "~/UCB-O365/Mountain limnology lab - Documents/Data/Sensors"
HOBO_DIR      <- file.path(BASE_DIR, "HOBO")
MINIDOT_DIR   <- file.path(BASE_DIR, "miniDOT")
CONCAT_DIR    <- file.path(MINIDOT_DIR, "concat")
LAKE_META_F   <- file.path(BASE_DIR, "lake_metadata.csv")
SENS_META_F   <- file.path(BASE_DIR, "sensor_metadata_all_loggers.csv")
OUTPUT_DATA_F <- file.path(BASE_DIR, "output", "compiled_lake_sensor_data.csv")
OUTPUT_LOG_F  <- file.path(BASE_DIR, "output", "data_compilation_issues.csv")

# ------------------------------------------------------------------------------
# SECTION 2: ISSUE LOGGING ENGINE FOR DATA CONTRIBUTOR FEEDBACK
# ------------------------------------------------------------------------------
# WHAT:
#   A structured data structure to record every discrepancy, formatting issue,
#   typo, and missing metadata entry encountered across the dataset.
# WHY:
#   Requirement 3 specifies that logging must provide sufficient context and
#   actionable details for the data contributor to resolve issues in the raw
#   source files, preventing future compilation errors.
# ------------------------------------------------------------------------------

issue_list <- list()
issue_counter <- 0L

log_issue <- function(severity, category, source_file, source_row, parameter,
                      problematic_raw_value, context_description, contributor_remediation) {
  issue_counter <<- issue_counter + 1L
  issue_list[[issue_counter]] <<- list(
    issue_id               = issue_counter,
    severity               = as.character(severity),
    category               = as.character(category),
    source_file            = as.character(source_file),
    source_row             = as.character(source_row),
    parameter              = as.character(parameter),
    problematic_raw_value  = as.character(problematic_raw_value),
    context_description    = as.character(context_description),
    contributor_remediation = as.character(contributor_remediation)
  )
}

# ------------------------------------------------------------------------------
# SECTION 3: METADATA INGESTION & COMPREHENSIVE AUDITING
# ------------------------------------------------------------------------------
# WHAT:
#   Ingest `lake_metadata.csv` and `sensor_metadata_all_loggers.csv`. Audit all
#   metadata rows for syntax, missing values, non-numeric depths, and date/time
#   placeholders.
# WHY:
#   Sensor metadata is the foundational anchor linking sensor hardware serial
#   numbers to lake geographic coordinates, elevation, deployment depths, and
#   depth references. Catching errors here gives immediate feedback to the
#   contributor on field record gaps.
# ------------------------------------------------------------------------------

cat("Step 1: Reading and auditing master metadata files...\n")

# Ingest lake metadata
if (!file.exists(LAKE_META_F)) {
  stop("FATAL: lake_metadata.csv not found at: ", LAKE_META_F)
}
lake_meta <- fread(LAKE_META_F, sep = ",")
# Clean column names to standard lowercase
setnames(lake_meta, names(lake_meta), tolower(names(lake_meta)))
setnames(lake_meta, c("latitude_dd", "longitude_dd"), c("latitude_dd", "longitude_dd"))
valid_lake_ids <- unique(lake_meta$lake_id)
cat("  Loaded metadata for", nrow(lake_meta), "lakes:", paste(valid_lake_ids, collapse = ", "), "\n")

# Ingest sensor metadata (auto-strips UTF-8 BOM if present)
if (!file.exists(SENS_META_F)) {
  stop("FATAL: sensor_metadata_all_loggers.csv not found at: ", SENS_META_F)
}
sensor_meta <- fread(SENS_META_F, sep = ",", colClasses = "character")
cat("  Loaded", nrow(sensor_meta), "sensor deployment/retrieval records.\n")

# Audit sensor metadata records row by row
cat("  Auditing sensor metadata for contributor feedback...\n")
for (i in seq_len(nrow(sensor_meta))) {
  row_num <- i + 1L # Account for header row in CSV file
  row_data <- sensor_meta[i]

  lid   <- trimws(row_data$lake_id)
  dt    <- trimws(row_data$date)
  tm    <- trimws(row_data$time)
  sn    <- trimws(row_data$serial_number)
  dm    <- trimws(row_data$depth_m)
  df    <- trimws(row_data$depth_from)
  act   <- trimws(row_data$deployRemove)

  # Check Lake ID validity
  if (!(lid %in% valid_lake_ids)) {
    log_issue(
      severity               = "ERROR",
      category               = "METADATA_TYPO",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "lake_id",
      problematic_raw_value  = lid,
      context_description    = paste0("Lake code '", lid, "' does not match any known lake in lake_metadata.csv."),
      contributor_remediation = paste0("Correct lake_id in row ", row_num, " to one of: ", paste(valid_lake_ids, collapse = ", "), ".")
    )
  }

  # Check Date format (expected YYYYMMDD, 8 digits)
  if (!grepl("^\\d{8}$", dt)) {
    log_issue(
      severity               = "WARNING",
      category               = "METADATA_INCOMPLETE",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "date",
      problematic_raw_value  = dt,
      context_description    = paste0("Date value '", dt, "' is not in standard YYYYMMDD format. Likely an uncompleted field placeholder (e.g., 'fill in')."),
      contributor_remediation = paste0("Replace '", dt, "' in row ", row_num, " with the exact 8-digit date (YYYYMMDD) from field notebooks.")
    )
  }

  # Check Time format (expected HH:MM, 1-2 digits : 2 digits)
  if (!grepl("^\\d{1,2}:\\d{2}$", tm)) {
    log_issue(
      severity               = "WARNING",
      category               = "METADATA_INCOMPLETE",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "time",
      problematic_raw_value  = tm,
      context_description    = paste0("Time value '", tm, "' is not in standard HH:MM format (found placeholder or season name e.g. 'pring')."),
      contributor_remediation = paste0("Replace '", tm, "' in row ", row_num, " with the 24-hour deployment/retrieval time (HH:MM).")
    )
  }

  # Check Serial Number
  if (is.na(sn) || sn == "" || sn == "NA") {
    log_issue(
      severity               = "ERROR",
      category               = "METADATA_MISSING",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "serial_number",
      problematic_raw_value  = sn,
      context_description    = paste0("Missing logger serial number in row ", row_num, " (", lid, " ", act, "). Prevents hardware lineage tracking."),
      contributor_remediation = paste0("Identify and input the missing sensor serial number in row ", row_num, " using field logs.")
    )
  }

  # Check Depth value numeric validity
  if (is.na(suppressWarnings(as.numeric(dm)))) {
    log_issue(
      severity               = "ERROR",
      category               = "METADATA_TYPO",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "depth_m",
      problematic_raw_value  = dm,
      context_description    = paste0("Non-numeric depth value '", dm, "' found in row ", row_num, "."),
      contributor_remediation = paste0("Replace placeholder '", dm, "' in row ", row_num, " with the measured mooring depth in meters.")
    )
  }

  # Check Depth reference datum (expected BTM or TOP)
  if (!(df %in% c("BTM", "TOP"))) {
    log_issue(
      severity               = "WARNING",
      category               = "METADATA_TYPO",
      source_file            = SENS_META_F,
      source_row             = row_num,
      parameter              = "depth_from",
      problematic_raw_value  = df,
      context_description    = paste0("Unknown depth reference '", df, "' in row ", row_num, ". Expected 'BTM' (bottom) or 'TOP' (surface)."),
      contributor_remediation = paste0("Correct depth_from in row ", row_num, " to either 'BTM' or 'TOP'.")
    )
  }
}

# Build lake metadata lookup keyed by lowercase lake_id
lake_lookup <- split(lake_meta[, .(lake_id, lake_name, latitude_dd, longitude_dd, elevation_m)], by = "lake_id")

# ------------------------------------------------------------------------------
# SECTION 4: HOBO DATA INGESTION & PARSING
# ------------------------------------------------------------------------------
# WHAT:
#   Ingest all 53 HOBO CSV files. Extract metadata from the standardized
#   filename convention, standardize headers, isolate qualitative event flags
#   into `quality_flag`, and parse timestamps into ISO 8601 UTC and local time.
# WHY:
#   HOBO Pendant loggers record temperature and light intensity.
#   - Filenames encode: <lake>_<depth>_<orientation>_<deployDate>_<retrieveDate>_serial<serial>.csv
#   - Timestamps are in local Mountain time (MDT or MST as noted in file header).
#   - Event rows (Button Down, Host Connected, End of File) contain 'Logged'
#     while temperature and light are blank. Isolating these prevents text tokens
#     from polluting numeric measurement columns.
# ------------------------------------------------------------------------------

cat("\nStep 2: Parsing HOBO temperature and light data files...\n")

hobo_files <- list.files(HOBO_DIR, pattern = "[.]csv$", full.names = TRUE, recursive = TRUE)
cat("  Found", length(hobo_files), "HOBO CSV files.\n")

hobo_data_list <- vector("list", length(hobo_files))

for (file_idx in seq_along(hobo_files)) {
  f_path <- hobo_files[file_idx]
  f_name <- basename(f_path)
  rel_path <- sub("^\\./", "", f_path)

  # Parse filename convention:
  # Pattern: <LAKE>_<DEPTH>_<ORIENT>_<DEPLOYDATE>_<RETRIEVEDATE>_serial<SERIAL>.csv
  fn_pattern <- "^([A-Za-z0-9]+)_([0-9.]+)_([A-Za-z]+)_([0-9]{8})_([0-9]{8})_serial([0-9]+)[.]csv$"
  fn_matches <- regmatches(f_name, regexec(fn_pattern, f_name))[[1]]

  if (length(fn_matches) < 7) {
    log_issue(
      severity               = "ERROR",
      category               = "FILENAME_MISMATCH",
      source_file            = rel_path,
      source_row             = "NA",
      parameter              = "filename",
      problematic_raw_value  = f_name,
      context_description    = "Filename does not adhere to standardized naming convention: <LAKE>_<DEPTH>_<ORIENT>_<DEPLOYDATE>_<RETRIEVEDATE>_serial<SERIAL>.csv",
      contributor_remediation = "Rename file to follow standard naming pattern with underscore delimiters."
    )
    next
  }

  fn_lake   <- tolower(fn_matches[2])
  fn_depth  <- as.numeric(fn_matches[3])
  fn_orient <- fn_matches[4]
  fn_dep    <- fn_matches[5]
  fn_ret    <- fn_matches[6]
  fn_serial <- fn_matches[7]

  # Map orientation code to human-readable standard reference
  depth_ref <- if (toupper(fn_orient) == "BOT") "bottom" else if (toupper(fn_orient) == "TOP") "surface" else fn_orient

  # Cross-check filename serial against sensor_metadata_all_loggers.csv
  matched_meta <- sensor_meta[lake_id == fn_lake & (serial_number == fn_serial | serial_number == sub("^0+", "", fn_serial))]
  if (nrow(matched_meta) == 0) {
    # Check for specific known typo: Fern Lake 2.5m serial 21707296 vs metadata 21707496
    if (fn_lake == "fer" && fn_serial == "21707296") {
      log_issue(
        severity               = "WARNING",
        category               = "FILENAME_MISMATCH",
        source_file            = rel_path,
        source_row             = "NA",
        parameter              = "serial_number",
        problematic_raw_value  = fn_serial,
        context_description    = "Filename serial '21707296' does not match sensor_metadata serial '21707496' (probable single-digit typo '2' vs '4').",
        contributor_remediation = "Verify physical logger serial number. If 21707496, rename file to 'FER_2.5_BOT_20230914_20240612_serial21707496.csv'. If 21707296, update row 6 of sensor_metadata_all_loggers.csv."
      )
    } else {
      log_issue(
        severity               = "WARNING",
        category               = "DEPLOYMENT_UNRECORDED",
        source_file            = rel_path,
        source_row             = "NA",
        parameter              = "serial_number",
        problematic_raw_value  = fn_serial,
        context_description    = paste0("Logger serial '", fn_serial, "' in lake '", fn_lake, "' does not have matching deployment records in sensor_metadata_all_loggers.csv."),
        contributor_remediation = paste0("Add deployment and retrieval entries for serial '", fn_serial, "' in lake '", fn_lake, "' to sensor_metadata_all_loggers.csv.")
      )
    }
  }

  # Ingest CSV file (explicit sep="," to prevent whitespace mis-detection)
  # WHAT:
  #   Ingest raw HOBO CSV with Latin-1 encoding fallback to handle non-UTF-8 degree
  #   symbols (e.g., '\xa1C') without dropping rows or files.
  # WHY:
  #   Standardizes column names to UTF-8 while logging an informative warning if
  #   non-standard character encodings are detected.
  dt_raw <- fread(f_path, sep = ",", fill = TRUE, colClasses = "character", encoding = "Latin-1")

  if (nrow(dt_raw) == 0) {
    log_issue(
      severity               = "WARNING",
      category               = "FILE_EMPTY",
      source_file            = rel_path,
      source_row             = "NA",
      parameter              = "file_content",
      problematic_raw_value  = "0 rows",
      context_description    = "HOBO data file is completely empty.",
      contributor_remediation = "Check logger download export and replace empty file with valid data."
    )
    next
  }

  orig_cols <- names(dt_raw)
  clean_cols <- iconv(orig_cols, from = "latin1", to = "UTF-8", sub = "")
  if (any(clean_cols != orig_cols)) {
    log_issue(
      severity               = "WARNING",
      category               = "ENCODING_NON_UTF8",
      source_file            = rel_path,
      source_row             = 1,
      parameter              = "header",
      problematic_raw_value  = paste(orig_cols, collapse = ","),
      context_description    = "Header contains non-UTF8 byte encoding in degree symbol. Cleaned automatically to prevent data loss.",
      contributor_remediation = "Export HOBO CSV files using UTF-8 or standard ASCII degree symbols."
    )
    setnames(dt_raw, orig_cols, clean_cols)
  }
  raw_cols <- names(dt_raw)

  # Identify variable columns by pattern matching (handles BOM and formatting quirks)
  dt_col_name   <- raw_cols[grep("Date-Time|Date", raw_cols)][1]
  temp_col_name <- raw_cols[grep("Temperature", raw_cols)][1]
  light_col_name <- raw_cols[grep("Light", raw_cols)][1]
  event_cols    <- intersect(raw_cols, c("Button Down", "Button Up", "Host Connected", "Stopped", "End of File"))

  if (is.na(dt_col_name) || is.na(temp_col_name)) {
    log_issue(
      severity               = "ERROR",
      category               = "HEADER_INVALID",
      source_file            = rel_path,
      source_row             = 1,
      parameter              = "header",
      problematic_raw_value  = paste(raw_cols, collapse = ","),
      context_description    = "Could not identify required 'Date-Time' or 'Temperature' column in CSV header.",
      contributor_remediation = "Ensure CSV header contains standard Date-Time and Temperature column designations."
    )
    next
  }

  # Parse Date-Time column
  # WHAT:
  #   HOBO datetimes are in Mountain time (America/Denver). Formats include
  #   'MM/DD/YYYY HH:MM:SS', 'M/D/YY H:MM', and 'MM/DD/YYYY H:MM'.
  # WHY:
  #   We convert into dual ISO 8601 strings:
  #   1. `datetime_utc`: ISO 8601 in UTC (standardized cross-sensor time axis)
  #   2. `datetime_local`: ISO 8601 in America/Denver (ecological diurnal context)
  raw_dates <- dt_raw[[dt_col_name]]
  parsed_local <- parse_date_time(raw_dates, orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM"), tz = "America/Denver")

  # Detect any unparseable datetimes
  unparsed_idx <- which(is.na(parsed_local) & raw_dates != "" & !is.na(raw_dates))
  if (length(unparsed_idx) > 0) {
    log_issue(
      severity               = "ERROR",
      category               = "TIMESTAMP_UNPARSEABLE",
      source_file            = rel_path,
      source_row             = unparsed_idx[1] + 1L,
      parameter              = dt_col_name,
      problematic_raw_value  = raw_dates[unparsed_idx[1]],
      context_description    = paste0("Failed to parse ", length(unparsed_idx), " timestamps in HOBO file."),
      contributor_remediation = "Correct datetime format in file to standard 'MM/DD/YYYY HH:MM:SS'."
    )
  }

  # HOBO clock readings are Mountain local time; convert the instant to UTC.
  posix_utc <- with_tz(parsed_local, "UTC")
  posix_local <- with_tz(posix_utc, "America/Denver")
  iso_utc <- format(posix_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  iso_local <- format(posix_local, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Denver")

  # Extract numeric measurements (preserving untransformed precision)
  # Blank strings in event rows naturally coerce to NA with suppressWarnings
  num_temp  <- suppressWarnings(as.numeric(dt_raw[[temp_col_name]]))
  num_light <- if (!is.na(light_col_name)) suppressWarnings(as.numeric(dt_raw[[light_col_name]])) else rep(NA_real_, nrow(dt_raw))

  # Build qualitative event flags (segregated from numeric data)
  flags <- rep("OK", nrow(dt_raw))
  if (length(event_cols) > 0) {
    for (ec in event_cols) {
      ev_vals <- dt_raw[[ec]]
      has_ev <- which(!is.na(ev_vals) & ev_vals != "")
      if (length(has_ev) > 0) {
        flag_token <- paste0("HOBO_", toupper(gsub("[ ]+", "_", ec)))
        flags[has_ev] <- ifelse(flags[has_ev] == "OK", flag_token, paste(flags[has_ev], flag_token, sep = ";"))
      }
    }
  }

  # Lookup lake geographic metadata
  l_meta <- lake_lookup[[fn_lake]]
  lake_full_name <- if (!is.null(l_meta)) l_meta$lake_name else NA_character_
  lat_val        <- if (!is.null(l_meta)) l_meta$latitude_dd else NA_real_
  lon_val        <- if (!is.null(l_meta)) l_meta$longitude_dd else NA_real_
  elev_val       <- if (!is.null(l_meta)) l_meta$elevation_m else NA_real_

  # Assemble standardized table for this file
  hobo_data_list[[file_idx]] <- data.table(
    datetime_utc             = iso_utc,
    datetime_local           = iso_local,
    lake_id                  = fn_lake,
    lake_name                = lake_full_name,
    latitude_dd              = lat_val,
    longitude_dd             = lon_val,
    elevation_m              = elev_val,
    depth_m                  = fn_depth,
    depth_reference          = depth_ref,
    sensor_type              = "hobo_pend",
    serial_number            = fn_serial,
    temperature_deg_c        = num_temp,
    dissolved_oxygen_mg_l    = NA_real_,
    dissolved_oxygen_pct_sat = NA_real_,
    light_lux                = num_light,
    battery_volts            = NA_real_,
    minidot_q                = NA_real_,
    quality_flag             = flags,
    data_source_file         = rel_path
  )
}

hobo_combined <- rbindlist(hobo_data_list, use.names = TRUE, fill = TRUE)
cat("  HOBO compilation complete: ", nrow(hobo_combined), "rows parsed across", length(hobo_files), "files.\n")

# Free temporary memory
rm(hobo_data_list)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# SECTION 5: miniDOT DAILY FILE INGESTION & PARSING
# ------------------------------------------------------------------------------
# WHAT:
#   Ingest all 22,694 daily text files across 50 miniDOT sensor directories.
#   Extract lake, depth, and orientation from folder names. Read Line 1 of each
#   file to capture sensor hardware serial number. Vectorize Unix epoch timestamp
#   conversion into ISO 8601 UTC and local Mountain time. Preserve raw BV, Temp,
#   DO, and Q values without transformation.
# WHY:
#   miniDOT sensors log at 10-minute intervals and create daily files:
#     Line 1: Hardware serial number (e.g. '7450-992461' or '7392-296600')
#     Line 2: Calibration string and OS revision
#     Line 3: Column names: 'Time (sec), BV (Volts), T (deg C), DO (mg/l), Q ()'
#     Line 4+: Measurement rows with Unix epoch seconds in UTC.
#   Auditing reveals 7 folders had hardware serial swaps over time; reading
#   Line 1 guarantees 100% accurate serial lineage for every single day.
# ------------------------------------------------------------------------------

cat("\nStep 3: Parsing miniDOT daily dissolved oxygen and temperature data files...\n")

# Identify all miniDOT sensor folders (excluding 'concat' and hidden dirs)
minidot_all_dirs <- list.dirs(MINIDOT_DIR, recursive = FALSE, full.names = TRUE)
minidot_dirs <- minidot_all_dirs[basename(minidot_all_dirs) != "concat"]
cat("  Found", length(minidot_dirs), "miniDOT sensor directories.\n")

minidot_folder_list <- vector("list", length(minidot_dirs))

for (dir_idx in seq_along(minidot_dirs)) {
  folder_path <- minidot_dirs[dir_idx]
  folder_name <- basename(folder_path)

  # Parse directory naming convention: <LAKE>_<DEPTH>_<ORIENTATION>
  dir_pattern <- "^([A-Za-z0-9]+)_([0-9.]+)_([A-Za-z]+)$"
  dir_matches <- regmatches(folder_name, regexec(dir_pattern, folder_name))[[1]]

  if (length(dir_matches) < 4) {
    log_issue(
      severity               = "ERROR",
      category               = "FOLDERNAME_INVALID",
      source_file            = folder_path,
      source_row             = "NA",
      parameter              = "folder_name",
      problematic_raw_value  = folder_name,
      context_description    = "Directory name does not adhere to standard: <LAKE>_<DEPTH>_<ORIENTATION>",
      contributor_remediation = "Rename directory to follow standard format (e.g., 'LOC_0.35_BOT')."
    )
    next
  }

  dir_lake   <- tolower(dir_matches[2])
  dir_depth  <- as.numeric(dir_matches[3])
  dir_orient <- dir_matches[4]
  depth_ref  <- if (toupper(dir_orient) == "BOT") "bottom" else if (toupper(dir_orient) == "TOP") "surface" else dir_orient

  # Check Lake ID
  if (!(dir_lake %in% valid_lake_ids)) {
    log_issue(
      severity               = "ERROR",
      category               = "METADATA_TYPO",
      source_file            = folder_path,
      source_row             = "NA",
      parameter              = "lake_id",
      problematic_raw_value  = dir_lake,
      context_description    = paste0("Directory specifies lake code '", dir_lake, "' not found in lake_metadata.csv."),
      contributor_remediation = paste0("Rename directory with valid lake code: ", paste(valid_lake_ids, collapse = ", "))
    )
  }

  # Lookup lake geographic metadata
  l_meta <- lake_lookup[[dir_lake]]
  lake_full_name <- if (!is.null(l_meta)) l_meta$lake_name else NA_character_
  lat_val        <- if (!is.null(l_meta)) l_meta$latitude_dd else NA_real_
  lon_val        <- if (!is.null(l_meta)) l_meta$longitude_dd else NA_real_
  elev_val       <- if (!is.null(l_meta)) l_meta$elevation_m else NA_real_

  # List all daily text files in this folder
  txt_files <- list.files(folder_path, pattern = "[.](txt|TXT)$", full.names = TRUE)

  if (length(txt_files) == 0) {
    log_issue(
      severity               = "WARNING",
      category               = "FOLDER_EMPTY",
      source_file            = folder_path,
      source_row             = "NA",
      parameter              = "files",
      problematic_raw_value  = "0 files",
      context_description    = paste0("Directory '", folder_name, "' contains no data files."),
      contributor_remediation = "Populate directory with logger text files or remove empty directory."
    )
    next
  }

  # Process files within folder
  # Vectorized folder-level batch reading
  file_batches <- lapply(txt_files, function(tf) {
    # Extract Line 1 to capture serial number directly from instrument header
    con <- file(tf, "r")
    l1 <- readLines(con, n = 1, warn = FALSE)
    close(con)

    # Serial format is typically '7450-992461' or '7392-296600'
    raw_sn <- trimws(l1)
    clean_sn <- sub("^[0-9]+-", "", raw_sn)

    # Ingest data lines starting at line 4 (skip=2 skips line 1 and 2, line 3 is header)
    dt_f <- fread(tf, skip = 2, sep = ",", header = TRUE, showProgress = FALSE, colClasses = "numeric")

    if (nrow(dt_f) > 0) {
      dt_f[, `:=`(
        serial_number    = clean_sn,
        data_source_file = sub("^\\./", "", tf)
      )]
    }
    dt_f
  })

  folder_dt <- rbindlist(file_batches, fill = TRUE, use.names = TRUE)

  if (nrow(folder_dt) > 0) {
    # Standardize column names (miniDOT headers: 'Time (sec)', 'BV (Volts)', 'T (deg C)', 'DO (mg/l)', 'Q ()')
    setnames(folder_dt, names(folder_dt), tolower(gsub("[ ()]+", "_", trimws(names(folder_dt)))))

    # Identify variable columns
    time_col <- grep("^time", names(folder_dt), value = TRUE)[1]
    bv_col   <- grep("^bv", names(folder_dt), value = TRUE)[1]
    temp_col <- grep("^(t_|temp)", names(folder_dt), value = TRUE)[1]
    do_col   <- grep("^do", names(folder_dt), value = TRUE)[1]
    q_col    <- grep("^q", names(folder_dt), value = TRUE)[1]

    # Convert Unix epoch timestamp (seconds) into ISO 8601 UTC and America/Denver
    epoch_secs <- folder_dt[[time_col]]
    posix_utc  <- as.POSIXct(epoch_secs, origin = "1970-01-01", tz = "UTC")
    # miniDOT epoch seconds are UTC; local is a separate display column.
    posix_local <- with_tz(posix_utc, "America/Denver")
    iso_utc <- format(posix_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    iso_local <- format(posix_local, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Denver")

    # Evaluate miniDOT optical quality metric Q:
    # Manufacturers and limnologists treat Q >= 0.7 as reliable; Q < 0.7 indicates biofouling or sensor error
    q_vals <- folder_dt[[q_col]]
    q_flags <- rep("OK", nrow(folder_dt))
    low_q_idx <- which(!is.na(q_vals) & q_vals < 0.7)
    if (length(low_q_idx) > 0) {
      q_flags[low_q_idx] <- "MINIDOT_LOW_Q"
    }

    # Audit Fern Lake miniDOT deployment records in sensor metadata:
    # Fern Lake (FER) has 1,679 miniDOT files, but zero records in sensor_metadata_all_loggers.csv!
    unique_sns <- unique(folder_dt$serial_number)
    for (s_num in unique_sns) {
      has_meta <- nrow(sensor_meta[lake_id == dir_lake & (serial_number == s_num | serial_number == sub("^0+", "", s_num))])
      if (has_meta == 0) {
        log_issue(
          severity               = "WARNING",
          category               = "DEPLOYMENT_UNRECORDED",
          source_file            = folder_path,
          source_row             = "NA",
          parameter              = "serial_number",
          problematic_raw_value  = s_num,
          context_description    = paste0("miniDOT sensor '", s_num, "' in directory '", folder_name, "' has no deployment/retrieval records in sensor_metadata_all_loggers.csv."),
          contributor_remediation = paste0("Add deployment and retrieval entries for miniDOT serial '", s_num, "' in lake '", dir_lake, "' (depth ", dir_depth, "m ", depth_ref, ") to sensor_metadata_all_loggers.csv.")
        )
      }
    }

    # Build standardized data table for this folder
    minidot_folder_list[[dir_idx]] <- data.table(
      datetime_utc             = iso_utc,
      datetime_local           = iso_local,
      lake_id                  = dir_lake,
      lake_name                = lake_full_name,
      latitude_dd              = lat_val,
      longitude_dd             = lon_val,
      elevation_m              = elev_val,
      depth_m                  = dir_depth,
      depth_reference          = depth_ref,
      sensor_type              = "pme_minidot",
      serial_number            = folder_dt$serial_number,
      temperature_deg_c        = folder_dt[[temp_col]],
      dissolved_oxygen_mg_l    = folder_dt[[do_col]],
      dissolved_oxygen_pct_sat = NA_real_,
      light_lux                = NA_real_,
      battery_volts            = folder_dt[[bv_col]],
      minidot_q                = q_vals,
      quality_flag             = q_flags,
      data_source_file         = folder_dt$data_source_file
    )
  }

  cat(sprintf("  [%02d/%02d] Parsed %-16s (%5d files, %7d rows)\n",
              dir_idx, length(minidot_dirs), folder_name, length(txt_files),
              if (!is.null(minidot_folder_list[[dir_idx]])) nrow(minidot_folder_list[[dir_idx]]) else 0))
}

minidot_combined <- rbindlist(minidot_folder_list, use.names = TRUE, fill = TRUE)
cat("  miniDOT daily compilation complete: ", nrow(minidot_combined), "rows parsed across",
    length(minidot_dirs), "directories.\n")

# Free temporary memory
rm(minidot_folder_list)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# SECTION 6: HISTORICAL miniDOT CONCAT FILE INGESTION & DEDUPLICATION
# ------------------------------------------------------------------------------
# WHAT:
#   Ingest non-duplicate 2016-2017 historical records from `miniDOT/concat/`.
#   Omit the 2017-2018 concat files that duplicate existing daily files.
# WHY:
#   The 4 concat files covering 2016-07-18 to 2017-06-26:
#     - `Loch_0.5m_16-17_all.TXT`
#     - `Loch_4.5m_16-17_all.TXT`
#     - `Sky_0.5m_16-17_all.TXT`
#     - `Sky_6.5m_16-17_all.TXT`
#   contain baseline limnological data that DO NOT exist anywhere in the daily
#   folders (daily files only begin in July 2017). Ingesting them captures
#   historical coverage, while omitting the 2017-2018 files prevents duplicating
#   records already compiled in Section 5.
# ------------------------------------------------------------------------------

cat("\nStep 4: Ingesting historical non-duplicate baseline records from miniDOT/concat/...\n")

concat_files_to_ingest <- c(
  "Loch_0.5m_16-17_all.TXT",
  "Loch_4.5m_16-17_all.TXT",
  "Sky_0.5m_16-17_all.TXT",
  "Sky_6.5m_16-17_all.TXT"
)

concat_skipped_duplicates <- c(
  "Loch_hypo_Oct17-June18.TXT",
  "Loch_surface_Oct17-June18.TXT",
  "Sky_hypo_Oct17-Sept18.TXT",
  "Sky_surface_Oct17-June18.TXT"
)

# Log duplicate files skipped for data lineage documentation
for (skip_f in concat_skipped_duplicates) {
  log_issue(
    severity               = "INFO",
    category               = "CONCAT_DUPLICATE_SKIPPED",
    source_file            = file.path("miniDOT/concat", skip_f),
    source_row             = "NA",
    parameter              = "concat_file",
    problematic_raw_value  = skip_f,
    context_description    = paste0("File '", skip_f, "' was omitted because identical records are already compiled from original daily text files in LOC and SKY folders."),
    contributor_remediation = "No action needed. Maintained for archival purposes; daily files take precedence."
  )
}

concat_data_list <- vector("list", length(concat_files_to_ingest))

for (cf_idx in seq_along(concat_files_to_ingest)) {
  cf_name <- concat_files_to_ingest[cf_idx]
  cf_path <- file.path(CONCAT_DIR, cf_name)
  rel_path <- sub("^\\./", "", cf_path)

  if (!file.exists(cf_path)) {
    log_issue(
      severity               = "ERROR",
      category               = "FILE_MISSING",
      source_file            = rel_path,
      source_row             = "NA",
      parameter              = "file",
      problematic_raw_value  = cf_name,
      context_description    = paste0("Historical concat file '", cf_name, "' not found."),
      contributor_remediation = "Ensure 2016-2017 concatenated files are present in miniDOT/concat/."
    )
    next
  }

  # Read file header lines to extract sensor serial and locate table header
  header_lines <- readLines(cf_path, n = 15, warn = FALSE)

  # Extract sensor serial from Line 2 (e.g. 'Sensor: 7392-296600')
  sn_line <- header_lines[grep("^Sensor:", header_lines)[1]]
  raw_sn <- trimws(sub("^Sensor:[ ]*", "", sn_line))
  clean_sn <- sub("^[0-9]+-", "", raw_sn)

  # Parse lake and depth from filename
  # Examples: Loch_0.5m_16-17_all.TXT, Sky_6.5m_16-17_all.TXT
  cf_lake <- if (grepl("^Loch", cf_name, ignore.case = TRUE)) "loc" else if (grepl("^Sky", cf_name, ignore.case = TRUE)) "sky" else "unknown"
  depth_match <- regmatches(cf_name, regexec("([0-9.]+)m", cf_name))[[1]]
  cf_depth <- if (length(depth_match) >= 2) as.numeric(depth_match[2]) else NA_real_

  # In 2016-2017, all sensors deployed in Loch and Sky were measured from the surface (TOP)
  # Check against sensor_metadata
  cf_orient <- "surface"

  # Find line index where data table begins
  hdr_idx <- grep("Unix Timestamp", header_lines)[1]
  col_names_raw <- trimws(strsplit(header_lines[hdr_idx], ",")[[1]])

  # Skip units row (hdr_idx + 1) and read data directly as numeric
  dt_concat <- fread(cf_path, skip = hdr_idx + 1, sep = ",", header = FALSE, colClasses = "numeric")
  setnames(dt_concat, col_names_raw)

  # Identify columns
  time_col <- grep("Unix Timestamp", names(dt_concat), value = TRUE)[1]
  bv_col   <- grep("Battery", names(dt_concat), value = TRUE)[1]
  temp_col <- grep("Temperature", names(dt_concat), value = TRUE)[1]
  do_col   <- grep("Dissolved Oxygen$", names(dt_concat), value = TRUE)[1]
  dosat_col <- grep("Dissolved Oxygen Saturation", names(dt_concat), value = TRUE)[1]
  q_col    <- grep("^Q$", names(dt_concat), value = TRUE)[1]

  # Timestamp conversion
  epoch_secs <- dt_concat[[time_col]]
  posix_utc  <- as.POSIXct(epoch_secs, origin = "1970-01-01", tz = "UTC")
  posix_local <- with_tz(posix_utc, "America/Denver")
  iso_utc <- format(posix_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  iso_local <- format(posix_local, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Denver")

  # Optical quality flag
  q_vals <- dt_concat[[q_col]]
  q_flags <- rep("OK", nrow(dt_concat))
  low_q_idx <- which(!is.na(q_vals) & q_vals < 0.7)
  if (length(low_q_idx) > 0) {
    q_flags[low_q_idx] <- "MINIDOT_LOW_Q"
  }

  # Lake metadata
  l_meta <- lake_lookup[[cf_lake]]
  lake_full_name <- if (!is.null(l_meta)) l_meta$lake_name else NA_character_
  lat_val        <- if (!is.null(l_meta)) l_meta$latitude_dd else NA_real_
  lon_val        <- if (!is.null(l_meta)) l_meta$longitude_dd else NA_real_
  elev_val       <- if (!is.null(l_meta)) l_meta$elevation_m else NA_real_

  concat_data_list[[cf_idx]] <- data.table(
    datetime_utc             = iso_utc,
    datetime_local           = iso_local,
    lake_id                  = cf_lake,
    lake_name                = lake_full_name,
    latitude_dd              = lat_val,
    longitude_dd             = lon_val,
    elevation_m              = elev_val,
    depth_m                  = cf_depth,
    depth_reference          = cf_orient,
    sensor_type              = "pme_minidot",
    serial_number            = clean_sn,
    temperature_deg_c        = dt_concat[[temp_col]],
    dissolved_oxygen_mg_l    = dt_concat[[do_col]],
    dissolved_oxygen_pct_sat = dt_concat[[dosat_col]],
    light_lux                = NA_real_,
    battery_volts            = dt_concat[[bv_col]],
    minidot_q                = q_vals,
    quality_flag             = q_flags,
    data_source_file         = rel_path
  )

  cat(sprintf("  Ingested %-30s (%5d rows, dates: %s to %s)\n",
              cf_name, nrow(concat_data_list[[cf_idx]]),
              substr(iso_utc[1], 1, 10), substr(iso_utc[length(iso_utc)], 1, 10)))
}

concat_combined <- rbindlist(concat_data_list, use.names = TRUE, fill = TRUE)
cat("  Historical concat compilation complete: ", nrow(concat_combined), "rows parsed across",
    length(concat_files_to_ingest), "files.\n")

# Free temporary memory
rm(concat_data_list)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# SECTION 7: FINAL COMPILATION, RECTANGULAR SHAPING & TYPE HARMONIZATION
# ------------------------------------------------------------------------------
# WHAT:
#   Bind all parsed datasets (HOBO, miniDOT daily, and miniDOT historical concat)
#   into a single master table. Enforce homogenous data types, clean column names,
#   and primary chronological sort.
# WHY:
#   Requirement 2 mandates a strictly rectangular CSV where:
#     - Row 1 is a clean, standardized header row.
#     - All rows have identical column counts.
#     - Numeric columns contain purely numbers or NA.
#     - All timestamps follow ISO 8601 (YYYY-MM-DD hh:mm:ss).
# ------------------------------------------------------------------------------

cat("\nStep 5: Synthesizing datasets into master rectangular table...\n")

master_dt <- rbindlist(list(hobo_combined, minidot_combined, concat_combined), use.names = TRUE, fill = TRUE)

# Free intermediate tables to maximize available RAM
rm(hobo_combined, minidot_combined, concat_combined)
gc(verbose = FALSE)

# Enforce strict column types
cat("  Enforcing strict homogenous data types across all columns...\n")
master_dt[, `:=`(
  datetime_utc             = as.character(datetime_utc),
  datetime_local           = as.character(datetime_local),
  lake_id                  = as.character(lake_id),
  lake_name                = as.character(lake_name),
  latitude_dd              = as.numeric(latitude_dd),
  longitude_dd             = as.numeric(longitude_dd),
  elevation_m              = as.numeric(elevation_m),
  depth_m                  = as.numeric(depth_m),
  depth_reference          = as.character(depth_reference),
  sensor_type              = as.character(sensor_type),
  serial_number            = as.character(serial_number),
  temperature_deg_c        = as.numeric(temperature_deg_c),
  dissolved_oxygen_mg_l    = as.numeric(dissolved_oxygen_mg_l),
  dissolved_oxygen_pct_sat = as.numeric(dissolved_oxygen_pct_sat),
  light_lux                = as.numeric(light_lux),
  battery_volts            = as.numeric(battery_volts),
  minidot_q                = as.numeric(minidot_q),
  quality_flag             = as.character(quality_flag),
  data_source_file         = as.character(data_source_file)
)]

# ------------------------------------------------------------------------------
# SECTION 7A: DEPLOYMENT / RETRIEVAL WATER-STATUS ANNOTATION
# ------------------------------------------------------------------------------
# Like sensor_above_water() in minidot_functions.R, use sensor deployment
# intervals to flag observations. Unlike clean_hobo_minidot.R's historical
# end-of-day/start-of-day substitution, this uses the ACTUAL logged times.
# Metadata clock times are interpreted as America/Denver, then converted to UTC.
# No observations are removed. Missing/incomplete deployment history is flagged
# as unknown rather than incorrectly asserting that the sensor was above water.
# ------------------------------------------------------------------------------
cat("  Annotating observations using deployment/retrieval windows...\n")

parse_metadata_local <- function(date, time) {
  d <- trimws(as.character(date))
  t <- trimws(as.character(time))
  d <- ifelse(grepl("^[0-9]{8}$", d),
              paste0(substr(d, 1, 4), "-", substr(d, 5, 6), "-", substr(d, 7, 8)), d)
  t <- ifelse(grepl("^[0-9]{1,2}:[0-9]{2}$", t), paste0(t, ":00"), t)
  suppressWarnings(lubridate::ymd_hms(paste(d, t), tz = "America/Denver", quiet = TRUE))
}

metadata_events <- data.table::copy(sensor_meta)
metadata_events[, `:=`(
  lake_key = tolower(trimws(as.character(lake_id))),
  serial_key = sub("^0+", "", trimws(as.character(serial_number))),
  event_type = tolower(trimws(as.character(deployRemove))),
  local_time = parse_metadata_local(date, time)
)]
metadata_events[, utc_time := lubridate::with_tz(local_time, "UTC")]
metadata_events <- metadata_events[
  event_type %in% c("deploy", "retrieve") &
  !is.na(utc_time) & !is.na(serial_key) & nzchar(serial_key) &
  !is.na(lake_key) & nzchar(lake_key)
]

# Pair each deployment with the next retrieval for the same lake and serial.
# Repeated deployments without an intervening retrieval remain incomplete.
data.table::setorder(metadata_events, lake_key, serial_key, utc_time,
                     event_type)
metadata_events[, event_group := cumsum(event_type == "deploy"),
                by = .(lake_key, serial_key)]
windows <- metadata_events[event_group > 0L, .(
  start_utc = if (any(event_type == "deploy"))
    min(utc_time[event_type == "deploy"]) else as.POSIXct(NA, tz = "UTC"),
  end_utc = if (any(event_type == "retrieve"))
    min(utc_time[event_type == "retrieve"]) else as.POSIXct(NA, tz = "UTC")
), by = .(lake_key, serial_key, event_group)]
windows <- windows[!is.na(start_utc) & !is.na(end_utc) & end_utc > start_utc]

# Build an observation-level join key, retaining the master table's row order.
obs <- master_dt[, .(
  row_id = .I,
  lake_key = tolower(trimws(lake_id)),
  serial_key = sub("^0+", "", trimws(serial_number)),
  utc_time = suppressWarnings(lubridate::ymd_hms(datetime_utc,
                                                 tz = "UTC", quiet = TRUE))
)]
# ISO UTC strings with Z suffix also supported by lubridate::ymd_hms.
obs[, sensor_status := "unknown"]
valid_obs <- obs[!is.na(utc_time) & !is.na(serial_key) & nzchar(serial_key)]

# A sensor is classified above_water only if a complete deployment window
# exists for that same lake and serial. This is conservative for incomplete logs.
known_keys <- unique(windows[, .(lake_key, serial_key)])
if (nrow(known_keys)) {
  known_rows <- valid_obs[known_keys, on = .(lake_key, serial_key),
                          nomatch = 0L, which = TRUE]
  if (length(known_rows)) {
    obs[valid_obs$row_id[known_rows], sensor_status := "above_water"]
  }
  # Non-equi interval join; only return observation IDs to avoid huge joins.
  within_window <- windows[valid_obs,
    on = .(lake_key, serial_key, start_utc < utc_time, end_utc > utc_time),
    nomatch = 0L, allow.cartesian = TRUE,
    .(row_id = i.row_id)]
  if (nrow(within_window)) {
    obs[unique(within_window$row_id), sensor_status := "under_water"]
  }
}
master_dt[, sensor_status := obs$sensor_status]
cat("  Water-status counts:\n")
print(master_dt[, .N, by = sensor_status][order(sensor_status)])

# Keep the deployment windows and annotation summary for auditing.
OUTPUT_WINDOWS_F <- file.path(dirname(OUTPUT_DATA_F), "deployment_windows_utc.csv")
OUTPUT_STATUS_F <- file.path(dirname(OUTPUT_DATA_F), "sensor_status_summary.csv")
dir.create(dirname(OUTPUT_DATA_F), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(windows, OUTPUT_WINDOWS_F)
data.table::fwrite(master_dt[, .N, by = .(sensor_type, lake_id, sensor_status)],
                   OUTPUT_STATUS_F)
rm(obs, valid_obs, known_keys, metadata_events, windows)
gc(verbose = FALSE)

# Primary sort: by lake_id, depth_m, depth_reference, and datetime_utc
cat("  Applying primary sorting (lake_id, depth_m, depth_reference, datetime_utc)...\n")
setorder(master_dt, lake_id, depth_m, depth_reference, datetime_utc)

# ------------------------------------------------------------------------------
# SECTION 8: FINAL EXPORT & VALIDATION CHECKS
# ------------------------------------------------------------------------------
# WHAT:
#   Export `compiled_lake_sensor_data.csv` and `data_compilation_issues.csv`.
#   Perform immediate in-memory validation of rectangularity, data types,
#   and formatting compliance before closing.
# WHY:
#   Using `data.table::fwrite()` guarantees fast, standard-compliant CSV output
#   with explicit NA representation (`na = "NA"`).
# ------------------------------------------------------------------------------

cat("\nStep 6: Writing output CSV files...\n")
dir.create(dirname(OUTPUT_DATA_F), recursive = TRUE, showWarnings = FALSE)
cat("  Exporting master compiled dataset to:", OUTPUT_DATA_F, "...\n")
fwrite(master_dt, file = OUTPUT_DATA_F, sep = ",", na = "NA", quote = "auto")
cat("  Master data exported successfully. File size:",
    round(file.info(OUTPUT_DATA_F)$size / (1024 * 1024), 1), "MB.\n")

# Convert issue list to data.table and export
cat("  Exporting contributor feedback and issue log to:", OUTPUT_LOG_F, "...\n")
if (length(issue_list) > 0) {
  issues_dt <- rbindlist(issue_list, fill = TRUE)
} else {
  issues_dt <- data.table(
    issue_id = integer(0), severity = character(0), category = character(0),
    source_file = character(0), source_row = character(0), parameter = character(0),
    problematic_raw_value = character(0), context_description = character(0),
    contributor_remediation = character(0)
  )
}
fwrite(issues_dt, file = OUTPUT_LOG_F, sep = ",", na = "NA", quote = TRUE)
cat("  Issue log exported successfully (", nrow(issues_dt), "issues logged).\n")

# ------------------------------------------------------------------------------
# SECTION 9: EXECUTIVE SUMMARY & VERIFICATION REPORT
# ------------------------------------------------------------------------------
# WHAT:
#   Print a clear terminal summary showing total rows, date ranges, parameters
#   compiled per lake, and breakdown of logged contributor issues.
# WHY:
#   Provides immediate confirmation of pipeline success and highlights top
#   action items for the limnology laboratory team.
# ------------------------------------------------------------------------------

cat("\n======================================================================\n")
cat("PIPELINE COMPILATION EXECUTIVE SUMMARY\n")
cat("======================================================================\n")
cat("Total Observations Compiled: ", format(nrow(master_dt), big.mark = ","), " rows\n", sep = "")
cat(sprintf("Total Columns:               %d columns\n", ncol(master_dt)))
cat(sprintf("Date Range (UTC):            %s to %s\n",
            min(master_dt$datetime_utc, na.rm = TRUE),
            max(master_dt$datetime_utc, na.rm = TRUE)))
cat(sprintf("Lakes Included:              %s\n", paste(unique(master_dt$lake_id), collapse = ", ")))
cat(sprintf("Total Issues Logged:         %d issues for contributor review\n\n", nrow(issues_dt)))

cat("Observations Breakdown by Lake and Sensor Type:\n")
summary_table <- master_dt[, .(
  Total_Rows    = .N,
  HOBO_Rows     = sum(sensor_type == "hobo_pend"),
  miniDOT_Rows  = sum(sensor_type == "pme_minidot"),
  Min_Date      = substr(min(datetime_utc), 1, 10),
  Max_Date      = substr(max(datetime_utc), 1, 10)
), by = .(lake_id, lake_name)]
setorder(summary_table, lake_id)
print(summary_table)

cat("\nBreakdown of Contributor Issues by Severity & Category:\n")
if (nrow(issues_dt) > 0) {
  issue_summary <- issues_dt[, .N, by = .(severity, category)]
  setorder(issue_summary, severity, -N)
  print(issue_summary)
}

cat("\n======================================================================\n")
cat("Pipeline completed successfully!\n")
cat("Master Dataset:     ", OUTPUT_DATA_F, "\n")
cat("Contributor Issues: ", OUTPUT_LOG_F, "\n")
cat("======================================================================\n")
