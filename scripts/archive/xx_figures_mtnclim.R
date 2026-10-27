#Making figures showing sulfate concentrations in The Loch, Sky Pond, Andrew's

source("scripts/00_libraries.R")

# file paths are incorrect - either fix paths, move data, or both - AGK 240827
# going to leave data where it is & update path 

# correct file path for master chem == "Data/Historical Loch Vale/water_chemistry/master_data/LVWS_waterchem_master.csv"

# file paths have been fixed! this works again - AGK 240827

# read in lvws data ------------------------
chem1 <-
  read.csv(
    "Data/Historical Loch Vale/water_chemistry/master_data/LVWS_waterchem_master.csv",
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
    "Data/Historical Loch Vale/water_chemistry/master_data/LVWS_2019_2020_master.csv",
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

str(chem1)
str(chem2)

#combine chem1 and chem2 to build dataframe - wanted to bind_rows upfront but R didn't want to.
water_chem <- bind_rows(chem1, chem2)
#this seems to work, but is adding a blank column at the end? not sure what's up with that
str(water_chem)

# filter for loch.o and norm samples -------------------------------
threelakes <- water_chem %>%
  filter(SITE %in% c("LOCH.O","SKY.O") & TYPE == "NORMAL") %>%
  mutate(MONTH = as.factor(MONTH))

library(ggdark)
threelakes %>%
  # filter(YEAR>1992) %>%
  filter(MONTH %in% c("7","8","9")) %>%
  group_by(YEAR, SITE) %>%
  summarize(SO4=median(SO4, na.rm=TRUE)) %>%
  mutate(SITE=case_when(SITE=="LOCH.O" ~ "The Loch",
                        SITE=="SKY.O" ~ "Sky Pond")) %>%
  ggplot(aes(x=YEAR, y=SO4))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm", color="grey50")+
  dark_theme_bw(base_size=18)+
  labs(y="Sulfate (mg/L)", #check units? Doesn't seem right
       x="Year",
       title="Loch Vale Watershed",
       caption="Data from: ScienceBase")+
  theme(legend.position = "none",
        axis.text.x=element_text(size=12))+
  scale_x_continuous(limits = c(1990,2020))+
  facet_wrap(~SITE)
ggsave("~/Library/CloudStorage/OneDrive-UCB-O365/Research/Data/R/FS-wilderness-lakes/plots/mtnclim24/sulfate_trends_LVWS.png",
       dpi=600, width=6, height=4, units="in")
