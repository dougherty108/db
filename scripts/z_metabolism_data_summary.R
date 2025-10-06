source("functions/00_helper_functions.R")
source("functions/03_miniDOT.R")

summary_LVWS <- combined_data_clean %>%
  mutate(waterYear = calcWaterYear(date_time),
         doy = yday(date_time)) %>%
  group_by(lake_id, waterYear) %>%
  summarise(n_obs = n_distinct(doy)) %>%
  arrange(waterYear) %>%
  pivot_wider(names_from = waterYear,
              values_from = n_obs)

# Pull in EDI data for GL4
# Package ID: knb-lter-nwt.175.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Dissolved oxygen data for the Green Lake 4 buoy, 2018 - ongoing..
# Data set creator:  Pieter T.J. Johnson -  
# Data set creator:  Samuel E. Yevak -  
# Data set creator:  Stephanie Dykema -  
# Data set creator:  Kelly A. Loria -  
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/175/7/f7348b7a7020dc2c8317af5fc0ada175" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "year",     
                 "sensor",     
                 "deployment",     
                 "timestamp",     
                 "depth",     
                 "temperature",     
                 "DO",     
                 "DO_saturation",     
                 "battery",     
                 "Q",     
                 "flag_temperature",     
                 "flag_DO",     
                 "flag_battery"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)
if (class(dt1$sensor)!="factor") dt1$sensor<- as.factor(dt1$sensor)
if (class(dt1$deployment)!="factor") dt1$deployment<- as.factor(dt1$deployment)                                   
# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(dt1$timestamp,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$timestamp != "",]) == length(tmp1timestamp[!is.na(tmp1timestamp)])){dt1$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$temperature)=="factor") dt1$temperature <-as.numeric(levels(dt1$temperature))[as.integer(dt1$temperature) ]               
if (class(dt1$temperature)=="character") dt1$temperature <-as.numeric(dt1$temperature)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]               
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$DO_saturation)=="factor") dt1$DO_saturation <-as.numeric(levels(dt1$DO_saturation))[as.integer(dt1$DO_saturation) ]               
if (class(dt1$DO_saturation)=="character") dt1$DO_saturation <-as.numeric(dt1$DO_saturation)
if (class(dt1$battery)=="factor") dt1$battery <-as.numeric(levels(dt1$battery))[as.integer(dt1$battery) ]               
if (class(dt1$battery)=="character") dt1$battery <-as.numeric(dt1$battery)
if (class(dt1$Q)=="factor") dt1$Q <-as.numeric(levels(dt1$Q))[as.integer(dt1$Q) ]               
if (class(dt1$Q)=="character") dt1$Q <-as.numeric(dt1$Q)
if (class(dt1$flag_temperature)!="factor") dt1$flag_temperature<- as.factor(dt1$flag_temperature)
if (class(dt1$flag_DO)!="factor") dt1$flag_DO<- as.factor(dt1$flag_DO)
if (class(dt1$flag_battery)!="factor") dt1$flag_battery<- as.factor(dt1$flag_battery)

# Convert Missing Values to NA for non-dates

dt1$temperature <- ifelse((trimws(as.character(dt1$temperature))==trimws("NaN")),NA,dt1$temperature)               
suppressWarnings(dt1$temperature <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$temperature))==as.character(as.numeric("NaN"))),NA,dt1$temperature))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NaN")),NA,dt1$DO)               
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NaN"))),NA,dt1$DO))
dt1$DO_saturation <- ifelse((trimws(as.character(dt1$DO_saturation))==trimws("NaN")),NA,dt1$DO_saturation)               
suppressWarnings(dt1$DO_saturation <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO_saturation))==as.character(as.numeric("NaN"))),NA,dt1$DO_saturation))
dt1$battery <- ifelse((trimws(as.character(dt1$battery))==trimws("NaN")),NA,dt1$battery)               
suppressWarnings(dt1$battery <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$battery))==as.character(as.numeric("NaN"))),NA,dt1$battery))
dt1$Q <- ifelse((trimws(as.character(dt1$Q))==trimws("NaN")),NA,dt1$Q)               
suppressWarnings(dt1$Q <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$Q))==as.character(as.numeric("NaN"))),NA,dt1$Q))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(year)
summary(sensor)
summary(deployment)
summary(timestamp)
summary(depth)
summary(temperature)
summary(DO)
summary(DO_saturation)
summary(battery)
summary(Q)
summary(flag_temperature)
summary(flag_DO)
summary(flag_battery) 
# Get more details on character variables

summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$sensor)) 
summary(as.factor(dt1$deployment)) 
summary(as.factor(dt1$flag_temperature)) 
summary(as.factor(dt1$flag_DO)) 
summary(as.factor(dt1$flag_battery))
detach(dt1)               







##Clean up data then graph
library(tidyverse)
library(lubridate)
data <- dt1 %>%
  mutate(datetime=ymd_hms(timestamp),
         doy=yday(datetime),
         waterYear=calcWaterYear(datetime))


summary_GL4 <- data %>%
  group_by(local_site, waterYear) %>%
  summarise(n_obs = n_distinct(doy)) %>%
  arrange(waterYear) %>%
  pivot_wider(names_from = waterYear,
              values_from = n_obs) %>%
  rename(lake_id=local_site)

summary_full <- bind_rows(summary_LVWS, summary_GL4)

library(huxtable)
quick_html(summary_full)

