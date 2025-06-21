source("scripts/00_libraries.R")

# read in loch outlet file and edit for nice exporting to csv

#read in usgs discharge data ----------------------------------------------
loch_discharge <- read.csv("Data/Loch Vale/usgs_data/loch_outlet/discharge/loch_discharge.txt", 
                             sep = "\t", header = FALSE, skip = 31) %>%
  select(3:5) %>%
  rename(date = 1, mean_discharge = 2, data_code = 3) %>%
  mutate(date = ymd(date))
str(loch_discharge)

#write discharge data to csv -----------------------------------------------
write_csv(loch_discharge, "Data/Loch Vale/usgs_data/loch_outlet/discharge/loch_discharge.csv", 
          col_names = TRUE)

loch_discharge %>%
  ggplot(aes(x = date, y = mean_discharge)) +
  geom_point() 
  geom_line()

str(loch_discharge)

#read in cond & temp data -----------------------------------------------
# pulling from 04_analysis_intermediate.R 
chem1 <-
  read.csv(
    "Data/Loch Vale/water_chemistry/master_data/LVWS_waterchem_master.csv",
    sep = ",",
    header = TRUE,
    skip = 1,
    na.strings = c("", " ", "NA")
  ) %>%
  select(1:19, 21:47) %>%
  rename(
    site_id = SITE.ID,
    "NA" = NA.,
    NH4_calc = NH4.calc,
    NO3_calc = NO3.calc,
    TDN_calc = TDN.calc,
    PO4_NREL_calc = PO4_NREL.calc,
    TP_NREL_calc = TP_NREL.calc
  ) %>%
  mutate(
    DATE = mdy(`DATE`),
    NO3_calc = case_when( #override old column
      NO3 == "<0.01" ~ 0.005,
      NO3 == "<0.02" ~ 0.01,
      NO3 == "<0.03" ~ 0.015,
      TRUE ~ as.numeric(NO3)
    ))

chem2 <-
  read.csv(
    "Data/Loch Vale/water_chemistry/master_data/LVWS_2019_2020_master.csv",
    sep = ",",
    skip = 1,
    header = TRUE,
    na.strings = c("", " ", "NA"),
    strip.white = TRUE
  ) %>%
  rename(
    site_id = SITE.ID,
    "NA" = NA.,
    NH4_calc = NH4.calc,
    NO3_calc = NO3.calc,
    TDN_calc = TDN.calc,
    PO4_NREL_calc = PO4_NREL.calc,
    TP_NREL_calc = TP_NREL.calc
  ) %>%
  mutate(
    DATE = mdy(`DATE`),
    NO3_calc = case_when( #override old column
      NO3 == "<0.01" ~ 0.005,
      NO3 == "<0.02" ~ 0.01,
      NO3 == "<0.03" ~ 0.015,
      TRUE ~ as.numeric(NO3)
    ))

str(chem1) 
str(chem2)

chem1 <- chem1 %>%
  mutate(across(TEMP:ncol(chem1), as.numeric))
chem2 <- chem2 %>%
  mutate(across(TEMP:ncol(chem2), as.numeric))
water_chem <- bind_rows(chem1, chem2)

# filter for loch.o and norm samples -----------------------------------
loch_o_chem <- water_chem %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL" & YEAR > 1995) %>%
  mutate(MONTH = as.factor(MONTH)) 

# filter temp & cond --------------------------------------------------
loch_cond_temp <- loch_o_chem %>%
  select("DATE", "TEMP", "FLDCOND")  %>%
  rename(date = DATE, temp = TEMP, field_cond = FLDCOND) 

#write to csv -------------------------------------------------------
write_csv(loch_cond_temp, "Data/Loch Vale/usgs_data/loch_outlet/conductivity/loch_cond_temp.csv", 
          col_names = TRUE)

# thinking about joining the two to create a df with temp, cond, and discharge. 
  # the issue is that it will leave a lot of NA values

locho_all <- left_join(loch_discharge, loch_cond_temp, by = "date")

loch_conductivity %>%
  ggplot(aes(x = DATE, y = FLDCOND)) +
  geom_point() +
  geom_line()

