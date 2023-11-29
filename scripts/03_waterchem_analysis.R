
source("scripts/00_libraries.R")

#read in file, rename some columns to remove spaces in column names. could all be renamed to be lowercase, i don't have a preference -AGK
#there's one blank column for whatever reason - using select() to drop it

#read in "master" file - missing 19/20
#adding new column for NO3_calc - overriding the if_else statement in the original excel file.
#AGK and IAO found an issue in the Excel formula

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
  
#read in 19_20 data
#see above comment about overriding if_else statement - manually calculating "NO3_calc"
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

str(chem1) #tons of columns coding as character; fix.
str(chem2)

#have to run this section or bind_rows won't work
chem1 <- chem1 %>%
  mutate(across(TEMP:ncol(chem1), as.numeric))

#You'll get a bunch of warnings, but I think it's fine.
chem2 <- chem2 %>%
  mutate(across(TEMP:ncol(chem2), as.numeric))

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
  #but starting in 1996 we consistently had at least 2 samples a month

loch_o_chem <- water_chem %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL" & YEAR > 1995) 

NO3_compare <- water_chem %>%
  filter(TYPE == "NORMAL") %>%
  select(1:7, 21:22, 34, 47)
#looks like there's only NO3_NREL data for 2015 & 2016



# Nitrate -----------------------------------------------------------------

#Plot a basic timeseries of nitrate
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = NO3_calc)) +
           geom_point() +
           geom_line()) 
           # geom_smooth(method = "lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NO3_calc)) +
           geom_point() +
           geom_line())

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NO3_calc)) +
           geom_point() +
           geom_line())

#IAO- Originally I was thinking of running linear models but clearly
#if there was a trend it is not linear. Need to mull that over.
#Something like a generalized additive model might be better but
#not sure the best way to compare across models

gam1_NO3 <- mgcv::gam(NO3_calc ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_NO3)
gratia::draw(gam1_NO3)

gam2_NO3 <- mgcv::gam(NO3_calc ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_NO3)
gratia::draw(gam2_NO3)

gam3_NO3 <- mgcv::gam(NO3_calc ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_NO3) 
gratia::draw(gam3_NO3)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_NO3) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_NO3) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_NO3) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


#Need to make predictions based on these models and compare across. 
#Will finish later.


# Calcium -----------------------------------------------------------------

#Plot a basic timeseries of CA
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = CA)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))



# Silica -----------------------------------------------------------------

#Plot a basic time series of silica
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = SiO2)) +
           geom_point() +
           geom_line() 
           # facet_wrap(~MONTH)
         )

#add other solutes @AGK

#Temperature --------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = TEMP)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TEMP)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TEMP)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))
#increasing trend in temperature isn't shown as much (at lesat visually) when sampling is only monthly - 
  #trend is preserved with biweekly sampling

#ANC ------------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = ANC)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = ANC)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = ANC)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#Conductivity (field) ------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = FLDCOND)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = FLDCOND)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = FLDCOND)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#Conductivity (lab) -------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = LABCOND)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = LABCOND)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = LABCOND)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#pH (field)
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = FLDPH)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = FLDPH)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = FLDPH)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))
#no pH data post-2010 😕

#pH (lab) -----------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = LABPH)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = LABPH)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = LABPH)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))
#all of the lab pH data is there - not sure what happened with the field pH data. 

#Mg --------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = MG)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = MG)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = MG)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#Sulfate ------------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = SO4)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = SO4)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = SO4)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#Chlorine ----------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = CL)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = CL)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = CL)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#DOC -----------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = DOC)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = DOC)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = DOC)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#TDN --------------------------------------------------------------
# not sure if this should be TDN or TDN_calc
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = TDN)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TDN)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TDN)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#Ammonium -------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = NH4_calc)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NH4_calc)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NH4_calc)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#TIN ------------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = TIN)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method = "lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TIN)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = TIN)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))

#DON -------------------------------------------------------------
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = DON)) +
           geom_point() +
           geom_line() +
           # facet_wrap(~MONTH, scales="free_y") +
           geom_smooth(method="lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = DON)) +
           geom_point() +
           geom_line() + 
           geom_smooth(method = "lm"))

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = DON)) +
           geom_point() +
           geom_line() +
           geom_smooth(method = "lm"))
#DON starts in 1999

























