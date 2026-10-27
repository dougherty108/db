#!/usr/bin/env Rscript
# Plot the single output CSV produced by compile_lake_sensor_data_utc_local.R
# Also works with compile_lake_sensor_data_utc_local_water_status.R.
# No other compilation output is needed.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

# ---- USER SETTINGS ----------------------------------------------------------
# Set this to the location of your compiled CSV, anywhere on your computer.
input_csv <- "~/UCB-O365/Mountain limnology lab - Documents/Data/Sensors/TEMPORARY/compiled_lake_sensor_data.csv"
# Save plots alongside the CSV by default (outside the Git project if desired).
output_dir <- file.path(dirname(input_csv), "compiled_lake_plots")
# "all" plots all records; "under_water" filters when sensor_status exists.
water_filter <- "all"
# Plot daily medians for readable, efficient long-term time series.
# Change to "raw" to plot individual observations (potentially very dense).
plot_resolution <- "daily"  # "daily" or "raw"
# ----------------------------------------------------------------------------

if (!file.exists(input_csv)) stop("Input CSV not found: ", input_csv)
if (!water_filter %in% c("all", "under_water")) stop("Invalid water_filter")
if (!plot_resolution %in% c("daily", "raw")) stop("Invalid plot_resolution")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

x <- fread(input_csv, na.strings = c("", "NA", "NaN"), showProgress = TRUE)
needed <- c("datetime_utc", "lake_id", "depth_m", "sensor_type", "temperature_deg_c")
missing <- setdiff(needed, names(x))
if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))

# datetime_utc was exported as a UTC ISO 8601 string. Parse explicitly in UTC;
# NEVER interpret it as local Mountain time or apply another offset.
# Handles YYYY-MM-DD HH:MM:SS, YYYY-MM-DDTHH:MM:SSZ and fractional seconds.
raw_time <- trimws(as.character(x$datetime_utc))
normalized <- sub("T", " ", raw_time, fixed = TRUE)
normalized <- sub("Z$", "", normalized)
# If the input includes explicit +/-HHMM offsets, honor them rather than strip them.
has_offset <- grepl("[+-][0-9]{2}:?[0-9]{2}$", normalized)
parsed <- as.POSIXct(rep(NA_character_, length(normalized)), tz = "UTC")
if (any(!has_offset)) {
  parsed[!has_offset] <- as.POSIXct(normalized[!has_offset], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}
if (any(has_offset)) {
  off <- normalized[has_offset]
  off <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", off)
  off <- sub(" ", "T", off, fixed = TRUE)
  # %z handles explicit numeric offsets; convert into absolute UTC instants.
  parsed[has_offset] <- as.POSIXct(strptime(off, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"))
}
x[, time_utc := parsed]
if (anyNA(x$time_utc)) warning(sum(is.na(x$time_utc)), " rows have unparseable UTC timestamps and will be omitted from plots")
x <- x[!is.na(time_utc)]

if (water_filter == "under_water") {
  if (!"sensor_status" %in% names(x)) stop("sensor_status column missing; use water_filter='all' or compile with the water-status script")
  x <- x[tolower(trimws(as.character(sensor_status))) == "under_water"]
}
if (!nrow(x)) stop("No observations remain after timestamp and water filters")

x[, lake_id := tolower(trimws(as.character(lake_id)))]
x[, sensor_type := as.character(sensor_type)]
x[, depth_m := as.numeric(depth_m)]
x[, depth_reference := if ("depth_reference" %in% names(x)) as.character(depth_reference) else "unspecified"]
x[, serial_number := if ("serial_number" %in% names(x)) as.character(serial_number) else "unknown"]
x[, depth_label := paste0(format(depth_m, trim = TRUE), " m (", depth_reference, ")")]

measures <- list(
  temperature = list(col = "temperature_deg_c", label = "Temperature (°C)"),
  oxygen = list(col = "dissolved_oxygen_mg_l", label = "Dissolved oxygen (mg/L)"),
  light = list(col = "light_lux", label = "Light (lux)")
)

# Export coverage to help distinguish missing data from differences in values.
coverage <- x[, .(n_records = .N,
                  first_utc = format(min(time_utc), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                  last_utc = format(max(time_utc), "%Y-%m-%d %H:%M:%S", tz = "UTC")),
              by = .(lake_id, sensor_type, depth_m, depth_reference, serial_number)]
fwrite(coverage, file.path(output_dir, "coverage_by_lake_depth_sensor.csv"))

for (metric in names(measures)) {
  spec <- measures[[metric]]
  if (!spec$col %in% names(x)) next
  kind <- if (metric == "light") "hobo_pend" else if (metric == "oxygen") "pme_minidot" else NA_character_
  z <- if (is.na(kind)) x else x[sensor_type == kind]
  z <- z[!is.na(get(spec$col))]
  if (!nrow(z)) next
  z <- z[, .(lake_id, sensor_type, depth_label, serial_number, time_utc,
             value = as.numeric(get(spec$col)))]
  if (plot_resolution == "daily") {
    z[, date_utc := as.Date(time_utc, tz = "UTC")]
    z <- z[, .(value = median(value, na.rm = TRUE), n = .N),
           by = .(lake_id, sensor_type, depth_label, serial_number, date_utc)]
    z[, plot_time := as.POSIXct(date_utc, tz = "UTC")]
  } else {
    z[, plot_time := time_utc]
  }
  z <- z[is.finite(value)]
  if (!nrow(z)) next
  for (lake in unique(z$lake_id)) {
    a <- z[lake_id == lake]
    p <- ggplot(a, aes(x = plot_time, y = value, color = serial_number,
                       group = interaction(serial_number, depth_label))) +
      geom_line(linewidth = 0.25, alpha = 0.7, na.rm = TRUE) +
      facet_grid(depth_label ~ sensor_type, scales = "free_y") +
      labs(title = paste(toupper(lake), "—", spec$label),
           subtitle = paste(if (plot_resolution == "daily") "Daily medians" else "Individual readings",
                            "| Time axis: UTC | Water filter:", water_filter),
           x = "Date/time (UTC)", y = spec$label, color = "Sensor serial") +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom")
    safe_lake <- gsub("[^A-Za-z0-9_-]", "_", lake)
    filename <- file.path(output_dir, paste0(safe_lake, "_", metric, "_", plot_resolution, ".png"))
    ggsave(filename, p, width = 13, height = max(5, 2.0 * uniqueN(a$depth_label) + 2),
           units = "in", dpi = 160, limitsize = FALSE)
    message("Saved: ", filename)
  }
}
message("Finished. Plots and coverage summary saved to: ", normalizePath(output_dir))
