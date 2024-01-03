
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

str(usgs_locho)
usgs_locho %>%
  mutate(sample_dt=dmy(sample_dt),
         nitrate=as.numeric(as.character(nitrate))) %>%
  ggplot(aes(x=sample_dt,y=nitrate))+
  geom_point()

no3_lvws <- water_chem %>%
  select(DATE, NO3_calc, NO3) %>%
  mutate(NO3_calc = NO3_calc*0.2259)
no3_usgs <- usgs_locho %>%
    mutate(sample_dt=mdy(sample_dt),
         nitrate=as.numeric(as.character(nitrate))) %>%
  select(sample_dt, nitrate)

no3_all <- full_join(no3_lvws, no3_usgs, by = c("DATE"="sample_dt"))

no3_all %>%
  ggplot(aes(x=nitrate, y=NO3))+
  geom_point()+
  geom_abline(slope=1, intercept=0)

no3_all %>%
  mutate(year=year(DATE)) %>%
  filter(DATE > "2016-01-01") %>%
  ggplot(aes(x=nitrate, y=NO3_calc, color=factor(year)))+
  geom_point()+
  geom_abline(slope=1, intercept=0)

no3_all %>%
  filter(DATE > "2018-01-01") %>%
  ggplot()+
  geom_point(aes(x=DATE, y=nitrate), color="blue", alpha=0.5)+
  geom_point(aes(x=DATE, y=NO3_calc), color="red", alpha=0.5)

# Nitrate -----------------------------------------------------------------

#Plot a basic timeseries of nitrate
ggplotly(loch_o_chem %>%
           ggplot(aes(x = DATE, y = NO3_calc*0.2259)) + #convert to NO3-N
           geom_point() +
           geom_line()) 
           # geom_smooth(method = "lm"))

#Same as above but sample bimonthly
ggplotly(loch_o_chem %>%
           group_by(MONTH,YEAR) %>%
           sample_n(size=2, replace=FALSE) %>%
           ggplot(aes(x = DATE, y = NO3_calc*0.2259)) + #convert to NO3-N
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

#creating month labels 
month_labels <- c("1" = "January", "2" = "February", "3" = "March", "4" = "April", 
                  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                  "9" = "September", "10" = "October", "11" = "November", "12" = "December")
# plot by month with linear trendlines / r squared

nitrate_linear_month <- ggplot(data = loch_o_chem, aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")


ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")

ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")




#need to reposition R2 and p
# okay that's marginally better

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) + 
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")

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

# GAM

gam1_CA <- mgcv::gam(CA ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_CA)
gratia::draw(gam1_CA)

gam2_CA <- mgcv::gam(CA ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_CA)
gratia::draw(gam2_CA)

gam3_CA <- mgcv::gam(CA ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_CA) 
gratia::draw(gam3_CA)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_CA) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_CA) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_CA) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")
  
# GAM
gam1_SiO2 <- mgcv::gam(SiO2 ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_SiO2)
gratia::draw(gam1_SiO2)

gam2_SiO2 <- mgcv::gam(SiO2 ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_SiO2)
gratia::draw(gam2_SiO2)

gam3_SiO2 <- mgcv::gam(SiO2 ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_SiO2) 
gratia::draw(gam3_SiO2)

bind_rows(
  broom::tidy(gam1_SiO2) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_SiO2) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_SiO2) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM
gam1_temp <- mgcv::gam(TEMP ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_temp)
gratia::draw(gam1_temp)

# error - non-positive values not allowed for the gamma family

gam2_temp <- mgcv::gam(TEMP ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_temp)
gratia::draw(gam2_temp)

gam3_temp <- mgcv::gam(TEMP ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_temp) 
gratia::draw(gam3_temp)

bind_rows(
  broom::tidy(gam1_temp) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_temp) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_temp) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM
gam1_anc <- mgcv::gam(ANC ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_anc)
gratia::draw(gam1_anc)

gam2_anc <- mgcv::gam(ANC ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_anc)
gratia::draw(gam2_anc)

gam3_anc <- mgcv::gam(ANC ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_anc) 
gratia::draw(gam3_anc)

bind_rows(
  broom::tidy(gam1_anc) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_anc) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_anc) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM

gam1_fcond <- mgcv::gam(FLDCOND ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_fcond)
gratia::draw(gam1_fcond)

gam2_fcond <- mgcv::gam(FLDCOND ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_fcond)
gratia::draw(gam2_fcond)

gam3_fcond <- mgcv::gam(FLDCOND ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_fcond) 
gratia::draw(gam3_fcond)

bind_rows(
  broom::tidy(gam1_fcond) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_fcond) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_fcond) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")

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

# GAM
gam1_lcond <- mgcv::gam(LABCOND ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_lcond)
gratia::draw(gam1_lcond)

gam2_lcond <- mgcv::gam(LABCOND ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_lcond)
gratia::draw(gam2_lcond)

gam3_lcond <- mgcv::gam(LABCOND ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_lcond) 
gratia::draw(gam3_lcond)

bind_rows(
  broom::tidy(gam1_lcond) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_lcond) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_lcond) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)

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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM

gam1_fpH <- mgcv::gam(FLDPH ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_fpH)
gratia::draw(gam1_fpH)

gam2_NO3 <- mgcv::gam(FLDPH ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_fpH)
gratia::draw(gam2_fpH)

gam3_fpH <- mgcv::gam(FLDPH ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_fpH) 
gratia::draw(gam3_fpH)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_fpH) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_fpH) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_fpH) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM
gam1_lpH <- mgcv::gam(LABPH ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_lpH)
gratia::draw(gam1_lpH)

gam2_lpH <- mgcv::gam(LABPH ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_lpH)
gratia::draw(gam2_lpH)

gam3_lpH <- mgcv::gam(LABPH ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_lpH) 
gratia::draw(gam3_lpH)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_lpH) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_lpH) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_lpH) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM

gam1_Mg <- mgcv::gam(MG ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_Mg)
gratia::draw(gam1_Mg)

gam2_Mg <- mgcv::gam(MG ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_Mg)
gratia::draw(gam2_Mg)

gam3_Mg <- mgcv::gam(MG ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_Mg) 
gratia::draw(gam3_Mg)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_Mg) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_Mg) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_Mg) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM

gam1_sulf <- mgcv::gam(SO4 ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_sulf)
gratia::draw(gam1_sulf)

gam2_sulf <- mgcv::gam(SO4 ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_sulf)
gratia::draw(gam2_sulf)

gam3_sulf <- mgcv::gam(SO4 ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_sulf) 
gratia::draw(gam3_sulf)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_sulf) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_sulf) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_sulf) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center") 

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

# GAM

gam1_Cl <- mgcv::gam(CL ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_Cl)
gratia::draw(gam1_Cl)

gam2_Cl <- mgcv::gam(CL ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_Cl)
gratia::draw(gam2_Cl)

gam3_Cl <- mgcv::gam(CL ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_Cl) 
gratia::draw(gam3_Cl)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_Cl) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_Cl) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_Cl) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")  

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

# GAM

gam1_DOC <- mgcv::gam(DOC ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_DOC)
gratia::draw(gam1_DOC)

gam2_DOC <- mgcv::gam(DOC ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_DOC)
gratia::draw(gam2_DOC)

gam3_DOC <- mgcv::gam(DOC ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_DOC) 
gratia::draw(gam3_DOC)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_DOC) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_DOC) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_DOC) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")  

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

# GAM
gam1_TDN <- mgcv::gam(TDN ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_TDN)
gratia::draw(gam1_TDN)

gam2_TDN <- mgcv::gam(TDN ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_TDN)
gratia::draw(gam2_TDN)

gam3_TDN <- mgcv::gam(TDN ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_TDN) 
gratia::draw(gam3_TDN)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_TDN) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_TDN) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_TDN) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")  

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

# GAM

gam1_amm <- mgcv::gam(NH4_calc ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_amm)
gratia::draw(gam1_amm)

gam2_amm <- mgcv::gam(NH4_calc ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_amm)
gratia::draw(gam2_amm)

gam3_amm <- mgcv::gam(NH4_calc ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_amm) 
gratia::draw(gam3_amm)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_amm) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_amm) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_amm) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")  

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

# GAM

gam1_TIN <- mgcv::gam(TIN ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_TIN)
gratia::draw(gam1_TIN)

gam2_TIN <- mgcv::gam(TIN ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_TIN)
gratia::draw(gam2_TIN)

gam3_TIN <- mgcv::gam(TIN ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_TIN) 
gratia::draw(gam3_TIN)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_TIN) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_TIN) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_TIN) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)


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
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = "center")  

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

# GAM
gam1_DON <- mgcv::gam(DON ~ s(DATE) + s(WEEK, bs = "cc", k = 52),
                       family=Gamma(link="log"),
                       data = loch_o_chem %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                       method = "REML")
broom::tidy(gam1_DON)
gratia::draw(gam1_DON)

gam2_DON <- mgcv::gam(DON ~  s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam2_DON)
gratia::draw(gam2_DON)

gam3_DON <- mgcv::gam(DON ~ s(DATE) + s(WEEK, bs ="cc", k=26),
                      family=Gamma(link="log"),
                      data = loch_o_chem %>%
                        group_by(MONTH,YEAR) %>%
                        sample_n(size=2, replace=FALSE) %>%
                        mutate(WEEK = isoweek(DATE),
                               DATE = decimal_date(DATE)),
                      method = "REML")
broom::tidy(gam3_DON) 
gratia::draw(gam3_DON)

#Pull together all the model results
bind_rows(
  broom::tidy(gam1_DON) %>%
    mutate(FREQ = "weekly"),
  broom::tidy(gam2_DON) %>%
    mutate(FREQ = "bimonthly"),
  broom::tidy(gam3_DON) %>%
    mutate(FREQ = "monthly")
) %>%
  arrange(term, FREQ)







