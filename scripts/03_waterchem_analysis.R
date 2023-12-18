
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
  filter(SITE == "LOCH.O" & TYPE == "NORMAL" & YEAR > 1995) %>%
  mutate(MONTH = as.factor(MONTH)) #%>%
  # mutate(YEAR = as.factor(YEAR))


NO3_compare <- water_chem %>%
  filter(TYPE == "NORMAL") %>%
  select(1:7, 21:22, 34, 47)
#looks like there's only NO3_NREL data for 2015 & 2016

#filter by loch_outlet; compare with published USGS NWIS data
NO3_locho <- NO3_compare %>%
  filter(SITE == "LOCH.O")

#read in NWIS data for loch_outlet
usgs_locho <- read.table("Data/Loch Vale/water_chemistry/master_data/usgs_locho_chem.txt", 
                       sep = "\t", header = TRUE, skip = 250) %>%
  rename(nitrate = p00618, nitrate_nitrite = p00631, nitrate_mg_L = p71851, 
         nitrate_micro = p91003) %>%
  select(1:5, nitrate, nitrate_nitrite, nitrate_mg_L, nitrate_micro)

#delete first row - could pipe this to above if you'd like, I couldn't figure it out. 

usgs_locho <- usgs_locho[-1,]

# "nitrate" is reported as mg/L as nitrogen, nitrate_mg_L is reported as nitrate mg/L as nitrate. 
# the I'm not entirely sure of the difference; neither seems to match up with our data
# nitrate_micro == micrograms per liter as nitrate

#I'm sure there's a slick way to compare these two dataframes; purely visually, these nitrate values
  # do not match what we have. it's entirely possible that I may have pulled the wrong parameter code 
    # or that I'm comparing values that are not equal. 



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

#facet by month
#color isn't really needed, can remove
ggplot(data = loch_o_chem, aes(x = DATE, y = NO3_calc, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

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

#linear model
lm1_NO3 <- ggplot(data = loch_o_chem, aes(x = DATE, y = NO3_calc)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_NO3

lm2_NO3 <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = NO3_calc)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_NO3

lm3_NO3 <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = NO3_calc)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_NO3


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

ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = CA)) +
           geom_point() +
           geom_line())

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NO3_calc)) +
           geom_point() +
           geom_line())

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = CA, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

# linear model 
lm1_CA <- ggplot(data = loch_o_chem, aes(x = DATE, y = CA)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_CA

lm2_CA <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = CA)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_CA             

lm3_CA <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = CA)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_CA

# Silica -----------------------------------------------------------------

#Plot a basic time series of silica
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = SiO2)) +
           geom_point() +
           geom_line() 
           # facet_wrap(~MONTH)
         )

ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = SiO2)) +
           geom_point() +
           geom_line())

#Same as above but sample monthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=1, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = SiO2)) +
           geom_point() +
           geom_line())

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = SiO2, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_SiO2 <- ggplot(data = loch_o_chem, aes(x = DATE, y = SiO2)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_SiO2

lm2_SiO2 <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = SiO2)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_SiO2

lm3_SiO2 <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = SiO2)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_SiO2

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
#increasing trend in temperature isn't shown as much (at least visually) when sampling is only monthly - 
  #trend is preserved with biweekly sampling

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = TEMP, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_temp <- ggplot(data = loch_o_chem, aes(x = DATE, y = TEMP)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_temp

lm2_temp <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = TEMP)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_temp

lm3_temp <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = TEMP)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_temp

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = ANC, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_anc <- ggplot(data = loch_o_chem, aes(x = DATE, y = ANC)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_anc

lm2_anc <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = ANC)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_anc

lm3_anc <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = ANC)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_anc

#trend seems to be artifically more linear when sampling monthly rather than weekly/biweekly


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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = FLDCOND, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model 
lm1_fcond <- ggplot(data = loch_o_chem, aes(x = DATE, y = FLDCOND)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_fcond

lm2_fcond <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = FLDCOND)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_fcond

lm3_fcond <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = FLDCOND)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_fcond


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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = LABCOND, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_lcond <- ggplot(data = loch_o_chem, aes(x = DATE, y = LABCOND)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_lcond

lm2_lcond <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = LABCOND)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_lcond

lm3_lcond <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = LABCOND)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_lcond

#pH (field) -----------------------------------------------------
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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = FLDPH, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_fpH <- ggplot(data = loch_o_chem, aes(x = DATE, y = FLDPH)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_fpH

lm2_fpH <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = FLDPH)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_fpH

lm3_fpH <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = FLDPH)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_fpH

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = LABPH, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_lpH <- ggplot(data = loch_o_chem, aes(x = DATE, y = LABPH)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_lpH

lm2_lpH <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = LABPH)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_lpH

lm3_lpH <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = LABPH)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_lpH

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = MG, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_Mg <- ggplot(data = loch_o_chem, aes(x = DATE, y = MG)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_Mg

lm2_Mg<- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = MG)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_Mg

lm3_Mg <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = MG)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_Mg

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = SO4, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_sulf <- ggplot(data = loch_o_chem, aes(x = DATE, y = SO4)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_sulf

lm2_sulf <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = SO4)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_sulf

lm3_sulf <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = SO4)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_sulf

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = CL, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_Cl <- ggplot(data = loch_o_chem, aes(x = DATE, y = CL)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_Cl

lm2_Cl <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = CL)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_Cl

lm3_Cl <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = CL)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_Cl


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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = DOC, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_DOC <- ggplot(data = loch_o_chem, aes(x = DATE, y = DOC)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_DOC

lm2_DOC <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = DOC)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_DOC

lm3_DOC <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = DOC)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_DOC

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = TDN, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_TDN <- ggplot(data = loch_o_chem, aes(x = DATE, y = TDN)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_TDN

lm2_TDN <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = TDN)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_TDN

lm3_TDN <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = TDN)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_TDN

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = NH4_calc, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model 
lm1_amm <- ggplot(data = loch_o_chem, aes(x = DATE, y = NH4_calc)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_amm

lm2_amm <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = NH4_calc)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_amm

lm3_amm <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = NH4_calc)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_amm

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = TIN, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_TIN <- ggplot(data = loch_o_chem, aes(x = DATE, y = TIN)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_TIN

lm2_TIN <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = TIN)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_TIN

lm3_TIN <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = TIN)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_TIN

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

#facet by month
ggplot(data = loch_o_chem, aes(x = DATE, y = DON, color = YEAR)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free") 

#linear model
lm1_DON <- ggplot(data = loch_o_chem, aes(x = DATE, y = DON)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm1_DON

lm2_DON <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
                  aes(x = DATE, y = DON)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm2_DON

lm3_DON <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
                  aes(x = DATE, y = DON)) +
          geom_point() + 
          geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01)
lm3_DON








