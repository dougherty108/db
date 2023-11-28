
source("scripts/00_libraries.R")

#read in file, rename some columns to remove spaces in column names. could all be renamed to be lowercase, i don't have a preference -AGK
#there's one blank column for whatever reason - using select() to drop it

#read in "master" file - missing 19/20
chem1 <-
  read.csv(
    "Data/Loch Vale/water_chemistry/master_data/LVWS_waterchem_master.csv",
    sep = ",",
    header = TRUE,
    skip = 1,
    na.strings=c(""," ","NA")
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
  mutate(DATE = mdy(`DATE`))
  
#read in 19_20 data
chem2 <- read.csv("Data/Loch Vale/water_chemistry/master_data/LVWS_2019_2020_master.csv",
                             sep = ",", skip = 1, header = TRUE, na.strings=c(""," ","NA"), strip.white = TRUE) %>%
    rename(
      site_id = SITE.ID, 
      "NA" = NA., 
      NH4_calc = NH4.calc, 
      NO3_calc = NO3.calc, 
      TDN_calc = TDN.calc, 
      PO4_NREL_calc = PO4_NREL.calc, 
      TP_NREL_calc = TP_NREL.calc) %>% 
    mutate(DATE = mdy(`DATE`))

str(chem1) #tons of columns coding as character; fix.
str(chem2)

chem1 <- chem1 %>%
  mutate(across(TEMP:TP_CSU, as.numeric))
#You'll get a bunch of warnings, but I think it's fine.
chem2 <- chem2 %>%
  mutate(across(TEMP:CHL_A, as.numeric))

#combine chem1 and chem2 to build dataframe - wanted to bind_rows upfront but R didn't want to.
water_chem <- bind_rows(chem1, chem2)
#this seems to work, but is adding a blank column at the end? not sure what's up with that

#quick notes on sampling below, can move elsewhere -AGK
  #sampling in 1981 is weekly through october, resumes in may 1982 - weekly throughout most of 1982
  #no sampling in jan, march, oct, or nov 1983 - weekly otherwise
  #sampling is at least monthly in 1984, weekly through the open-water period, no sampling in nov 1984
  #no sampling in feb-march or dec of 1985, at least monthly outside of that
  #no sampling in feb, oct, or dec 1986, sampling is weekly through the open water period
  #nearly complete weekly sampling through 1987 and 1988
  #no sampling between march and august 1989
  #weekly sampling begins in 1991

loch_o_chem <- water_chem %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL")

NO3_compare <- water_chem %>%
  filter(TYPE == "NORMAL") %>%
  select(1:7, 21:22, 34)
#looks like there's only NREL data for 2015 & 2016

# Are NO3 and NO3_calc correlated?
loch_o_chem %>%
  ggplot(aes(x=NO3_calc, y=NO3))+
  geom_abline(y=1,x=0,color="green")+ #1:1 line
  geom_point() +
#1:1 relationship for all samples but not all. WHY?

loch_o_chem %>%
  filter(NO3_calc < 1) %>%
  ggplot(aes(x=NO3_calc, y=NO3, color=YEAR))+
  geom_abline(y=1,x=0,color="green")+ #1:1 line
  geom_jitter(width = 0.1) #jittered the points to make it easier to see

#Not good. I would guess that in more recent years, a different correction
#factor was used? 

loch_o_chem %>%
  ggplot(aes(x=DATE, y=NO3))+
  geom_point() +

loch_o_chem %>%
  ggplot(aes(x=DATE, y=NO3_calc))+
  geom_point()

#What years are we missing NO3 data?
years <- loch_o_chem %>%
  select(YEAR, NO3) %>%
  group_by(YEAR) %>%
  summarize(n = length(unique(NO3)))
#Missing 2019 and 2020 data. 
  #fixed - has been added! 
















