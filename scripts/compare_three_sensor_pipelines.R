# Compare Colin/Gemini, Oleksy, and db lake sensor compilation outputs.
# Input files can live outside your R project / Git repository.
# Change input_dir to the folder containing the three ZIP files and two CSVs.
# Use forward slashes on Windows, macOS, or Linux.
# Example macOS: input_dir <- "/Users/yourname/Documents/lake_sensor_data"
# Example Windows: input_dir <- "D:/lake_sensor_data"
# Optional interactive folder selection (RStudio):
# input_dir <- rstudioapi::selectDirectory()
input_dir <- "~/Desktop"

# Keep generated reports in your current R project by default.
# Set output_dir to another absolute path if you prefer.
output_dir <- file.path(getwd(), "pipeline_comparison_results")

# install.packages(c("data.table", "ggplot2"))
library(data.table)
library(ggplot2)
options(datatable.print.nrows=20)

if (!dir.exists(input_dir)) {
  stop("Input folder does not exist: ", input_dir,
       "\nEdit input_dir near the top of this script.")
}
inputs <- list(
  colin = file.path(input_dir, "Colin_and_Gemini-20261027T194554Z-1-001.zip"),
  oleksy = file.path(input_dir, "Olesky.zip"),
  db = file.path(input_dir, "db.zip"),
  db_hobo = file.path(input_dir, "hobo_all_lakes_20261016.csv"),
  db_minidot = file.path(input_dir, "minidot_all_lakes_20261015.csv")
)
missing_inputs <- unlist(inputs)[!file.exists(unlist(inputs))]
if (length(missing_inputs)) {
  stop("Missing input files:\n", paste(missing_inputs, collapse = "\n"),
       "\nCheck input_dir and filenames in inputs.")
}
out <- output_dir
dir.create(out, showWarnings = FALSE, recursive = TRUE)
work <- file.path(out,'extracted');dir.create(work,showWarnings=FALSE)
# Extract only relevant compiled outputs, not the tens of thousands of raw files.
unzip(inputs$colin,files='Colin_and_Gemini/compiled_lake_sensor_data.csv.zip',exdir=work)
inner <- file.path(work,'Colin_and_Gemini','compiled_lake_sensor_data.csv.zip')
unzip(inner,files='compiled_lake_sensor_data.csv',exdir=work)
files_o <- unzip(inputs$oleksy,list=TRUE)$Name
files_o <- files_o[grepl('^Olesky/data/processed/.*_(water_temperature_hobo|dissolved_oxygen_minidot)\\.csv$',files_o)]
unzip(inputs$oleksy,files=files_o,exdir=work)
# All timestamps are compared as UTC instants, not naive clock strings.
# db CSVs: miniDOT date_time is America/Denver (as written by with_tz);
# HOBO date_time is interpreted as local clock time by default in db scripts.
# Set db_hobo_time_basis to 'UTC' if independent inspection proves otherwise.
db_hobo_time_basis <- 'America/Denver'
# For local wall-clock strings, ambiguous fall-back times cannot be uniquely
# recovered without source row order/offset. Keep ambiguous records flagged and
# omit them from exact UTC joins. Spring-forward nonexistent times are also flagged.
parse_utc <- function(x, basis='UTC') {
  s <- trimws(as.character(x)); s[s %in% c('','NA','NaN')] <- NA_character_
  s <- sub('T',' ',s,fixed=TRUE)
  # If explicit Z or numeric offset is present, respect it (do not relocalize).
  has_zone <- grepl('(Z|[+-][0-9]{2}:?[0-9]{2})$',s)
  z <- as.POSIXct(rep(NA_real_,length(s)),origin='1970-01-01',tz='UTC')
  if(any(has_zone,na.rm=TRUE)) {
    v <- s[has_zone]; v <- sub('Z$','+0000',v);v <- sub('([+-][0-9]{2}):([0-9]{2})$','\\1\\2',v)
    z[has_zone] <- as.POSIXct(v,format='%Y-%m-%d %H:%M:%OS%z',tz='UTC')
  }
  idx <- which(!has_zone & !is.na(s))
  if(length(idx)) {
    naive <- as.POSIXct(s[idx],format='%Y-%m-%d %H:%M:%OS',tz=basis)
    # Roundtrip detects nonexistent local clock hours, not ambiguous hours.
    back <- format(naive,'%Y-%m-%d %H:%M:%S',tz=basis)
    requested <- substr(s[idx],1,19)
    naive[is.na(back) | back != requested] <- as.POSIXct(NA)
    z[idx] <- naive
    if(basis=='America/Denver') {
      # Conservative: mark 01:00-01:59 on first Sunday in November as ambiguous.
      d <- as.Date(substr(s[idx],1,10)); lt <- as.POSIXlt(d,tz='UTC')
      amb <- !is.na(d) & lt$mon==10 & lt$wday==0 & lt$mday<=7 & substr(s[idx],12,13)=='01'
      z[idx[which(amb)]] <- as.POSIXct(NA)
    }
  }
  as.numeric(z)
}
standardize <- function(dt, source, kind) {
  n <- names(dt); choose <- function(...) {candidates <- c(...); a <- candidates[candidates %in% n];if(length(a)) a[1] else NA_character_}
  get <- function(...) {a <- choose(...);if(is.na(a)) rep(NA_character_,nrow(dt)) else as.character(dt[[a]])}
  lake <- tolower(trimws(get('lake_id','siteID')))
  serial <- get('sensor_serial','serial_number','serial_num_sub','serial_num')
  # db miniDOT uses a raw serial string and a shortened serial_num_sub.
  # Normalize leading zeros only; do not assume shortened IDs equal full IDs.
  serial <- gsub('^0+','',trimws(serial))
  depth <- suppressWarnings(as.numeric(get('depth_m','depth')))
  ref <- tolower(get('depth_reference','depth_from'))
  ref[ref %in% c('bot','btm')] <- 'bottom';ref[ref=='top'] <- 'surface'
  basis <- if(source=='db' && kind=='minidot') 'America/Denver' else if(source=='db') db_hobo_time_basis else 'UTC'
  stamp <- parse_utc(get('datetime_utc','date_time'),basis)
  data.table::data.table(source=source,kind=kind,lake=lake,serial=serial,depth=depth,depth_reference=ref,
             utc=stamp,temp=suppressWarnings(as.numeric(get('temperature_deg_c','water_temp_degC','temp'))),
             oxygen=suppressWarnings(as.numeric(get('dissolved_oxygen_mg_l','DO_mgL','do_obs'))),
             light=suppressWarnings(as.numeric(get('light_lux','lux'))),
             file=get('data_source_file','source_file'))
}
# Read one dataset at a time; retain only standardized fields.
cat('Reading Colin/Gemini...\n')
c <- data.table::fread(file.path(work,'compiled_lake_sensor_data.csv'),showProgress=TRUE)
c_kind <- ifelse(grepl('hobo',paste(c$sensor_type,c$data_source_file),ignore.case=TRUE),'hobo','minidot')
c[,pipeline_kind:=c_kind]; rm(c_kind)
colin <- data.table::rbindlist(lapply(c('hobo','minidot'),function(k) standardize(c[pipeline_kind==k],'colin',k)))
rm(c);gc()
cat('Reading Oleksy...\n')
oleksy <- rbindlist(lapply(files_o,function(f) {
  k <- if(grepl('water_temperature_hobo',f)) 'hobo' else 'minidot'
  standardize(fread(file.path(work,f),showProgress=FALSE),'oleksy',k)
}),use.names=TRUE)
cat('Reading db...\n')
db <- rbindlist(list(standardize(fread(inputs$db_hobo),'db','hobo'),standardize(fread(inputs$db_minidot),'db','minidot')))
all <- rbindlist(list(colin,oleksy,db));rm(colin,oleksy,db);gc()
all[,utc_string:=format(as.POSIXct(utc,origin='1970-01-01',tz='UTC'),'%Y-%m-%d %H:%M:%S',tz='UTC')]
# Coverage and parsing diagnostics.
coverage <- all[,.(rows=.N,invalid_utc=sum(is.na(utc)),missing_serial=sum(is.na(serial)|serial==''),
                   lakes=uniqueN(lake),sensors=uniqueN(serial),first_utc=if(all(is.na(utc))) NA_character_ else min(utc_string,na.rm=TRUE),
                   last_utc=if(all(is.na(utc))) NA_character_ else max(utc_string,na.rm=TRUE)),by=.(source,kind,lake)]
fwrite(coverage,file.path(out,'coverage_by_lake.csv'))
# Distinguish identical instants and measurements from duplicate keys.
key <- c('kind','lake','serial','depth','depth_reference','utc')
valid <- all[!is.na(utc)&!is.na(depth)&!is.na(serial)&serial!=''&!is.na(lake)&lake!='']
dups <- valid[,.(n=.N),by=c('source',key)][n>1]
fwrite(dups,file.path(out,'duplicate_keys.csv'))
# Add within-key occurrence index so duplicated timestamps never create Cartesian explosions.
setorderv(valid,c('source',key,'temp','oxygen','light'),na.last=TRUE)
valid[,occurrence:=seq_len(.N),by=c('source',key)]
# Exact matches for each pair; unmatched rows reported independently.
pairs <- list(c('colin','oleksy'),c('colin','db'),c('oleksy','db'))
for(p in pairs) {
  a <- valid[source==p[1]]; b <- valid[source==p[2]]
  setkeyv(a,c(key,'occurrence'));setkeyv(b,c(key,'occurrence'))
  m <- merge(a,b,by=c(key,'occurrence'),all=TRUE,suffixes=c('_a','_b'))
  m[,status:=fifelse(is.na(source_a),'only_b',fifelse(is.na(source_b),'only_a','matched'))]
  m[,`:=`(temp_difference=temp_a-temp_b,oxygen_difference=oxygen_a-oxygen_b,light_difference=light_a-light_b)]
  prefix <- paste(p,collapse='_vs_')
  fsum <- m[,.(matched=sum(status=='matched'),only_a=sum(status=='only_a'),only_b=sum(status=='only_b'),
               temp_comparable=sum(!is.na(temp_difference)),temp_changed=sum(!is.na(temp_difference)&abs(temp_difference)>1e-6),
               temp_max_abs=if(all(is.na(temp_difference))) NA_real_ else max(abs(temp_difference),na.rm=TRUE),
               oxygen_comparable=sum(!is.na(oxygen_difference)),oxygen_changed=sum(!is.na(oxygen_difference)&abs(oxygen_difference)>1e-6),
               oxygen_max_abs=if(all(is.na(oxygen_difference))) NA_real_ else max(abs(oxygen_difference),na.rm=TRUE)),by=.(kind,lake,depth)]
  fwrite(fsum,file.path(out,paste0(prefix,'_summary.csv')))
  # Full mismatch files can be large; still write them for reproducibility.
  fwrite(m[status!='matched',.(kind,lake,serial,depth,depth_reference,utc_string=as.character(as.POSIXct(utc,origin='1970-01-01',tz='UTC')),status)],file.path(out,paste0(prefix,'_unmatched.csv')))
  fwrite(m[status=='matched' & ((!is.na(temp_difference)&abs(temp_difference)>1e-6)|(!is.na(oxygen_difference)&abs(oxygen_difference)>1e-6)),
           .(kind,lake,serial,depth,utc,temp_a,temp_b,temp_difference,oxygen_a,oxygen_b,oxygen_difference)],file.path(out,paste0(prefix,'_value_disagreements.csv')))
  for(v in c('temp','oxygen')) {
    x <- m[status=='matched' & is.finite(get(paste0(v,'_a')))&is.finite(get(paste0(v,'_b')))]
    if(nrow(x)>0) {
      if(nrow(x)>100000) x <- x[sample.int(.N,100000)]
      g <- ggplot(x,aes(x=.data[[paste0(v,'_a')]],y=.data[[paste0(v,'_b')]]))+geom_point(alpha=.15,size=.3)+
        geom_abline(slope=1,intercept=0,color='red')+facet_wrap(~lake,scales='free')+
        labs(x=p[1],y=p[2],title=paste(prefix,v,'(UTC-matched observations)'))+theme_bw()
      ggsave(file.path(out,paste0(prefix,'_',v,'_scatter.png')),g,width=13,height=9,dpi=160)
    }
  }
  rm(m,a,b);gc()
}
fwrite(all[,.(source,kind,lake,serial,depth,depth_reference,utc_string,temp,oxygen,light)],file.path(out,'standardized_all_utc.csv'))
cat('Completed. Results in: ',out,'\n')
