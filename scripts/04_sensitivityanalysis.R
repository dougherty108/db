# creating new script for figures, etc. for abridged lvws sensitivity analysis/mtnclimn talk -AGK 240827

# source library and intermediate script 
source("scripts/00_libraries.R")
source("scripts/04_analysis_intermediate.R")

# analytes needed are as follows: 
  # increasing: temp, ANC, pH, conductivity 
  # decreasing: nitrate, TDN

# need plots, dropped frequency, table of results, and density dist for all 
# all plots being pulled straight from rmarkdown - plots will need to be named 

# WATER TEMP ---------------------------------------------------------------
# dropping sampling frequency plots by month 
  # full record
temp_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = TEMP)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") +
  labs(x = "Year", y = "Temperature") +
  theme_pubclean()
temp_full
# pulled pretty much straight from the rmarkdown doc w minimal issue. getting some odd error about non-finite & values 
  # outside the scale range - will look further into this later
# has a really weird aspect ratio when zoomed - could be a formatting relic from the rmarkdown doc 
temp_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = TEMP)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Temperature") +
  theme_pubclean()
temp_biweekly

temp_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = TEMP)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Temperature") +
  theme_pubclean()
temp_monthly

# will pull linear results from the linear_results tibble for each solute 

# density distribution 
  # combine all samples into new dataframe
  # some of these "samples" are values that were selected with reduced sampling frequency 
  # being added to the same df & indicating which sampling frequency they came from 
temp_all <- bind_rows(loch_o_chem %>%
  select(TEMP, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(TEMP) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(TEMP) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 
# density dist for all samples 
temp_density <- ggplot(temp_all, aes(TEMP, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="Temperature (ºC)",
       title="All samples")
temp_density

# density dist by month 
temp_density_monthly <- ggplot(temp_all, aes(TEMP, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="Temperature (ºC)",
       title="All samples")
temp_density_monthly
# again, works with minimal manipulation from the rmarkdown doc but formatting is weird in zoom window 

# ridgelines 

# I swear I had a full version of this with all seasons. will need to find or rewrite. 

# whole year
temp_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = TEMP, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Temperature (ºC)",
       y="Year",
       title="Year-round samples")
temp_ridgeline

# summer only
temp_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = TEMP, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Temperature (ºC)",
       y="Year",
       title="Summer only (July-August)")
temp_ridgeline_summer

# fall only 

# winter only 

# spring only 

# ANC --------------------------------------------------------------------
  # full record
anc_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = ANC)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") +
  labs(x = "Year", y = "ANC") +
  theme_pubclean()
anc_full
# some values outside the scale range; fewer than temp

# biweekly 
anc_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = ANC)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "ANC") +
  theme_pubclean()
anc_biweekly

# monthly
anc_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = ANC)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "ANC") +
  theme_pubclean()
anc_monthly

# density distribution 
# combine all data into one df for ANC
anc_all <- bind_rows(loch_o_chem %>%
  select(ANC, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(ANC) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(ANC) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 

# plot all samples 
anc_density <- ggplot(anc_all, aes(ANC, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="ANC",
       title="All samples")
anc_density

# plot by month 
anc_density_monthly <- ggplot(anc_all, aes(ANC, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="ANC",
       title="All samples")
anc_density_monthly

# ridgelines 
# again, will need all seasons for this. will write later - AGK 240828
anc_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = ANC, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="ANC",
       y="Year",
       title="Year-round samples")
anc_ridgeline
# unknown parameters, values outside scale range, etc etc
  # look more into this later - AGK 

anc_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = ANC, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="ANC",
       y="Year",
       title="Summer only (July-August)")
anc_ridgeline_summer

# fall only 

# winter only 

# spring only

# pH -------------------------------------------------------------------
# this is lab pH rather than field - I believe we deemed this record more reliable 
# full record
pH_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = LABPH)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") +
  labs(x = "Year", y = "pH (lab)") +
  theme_pubclean()
pH_full

pH_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = LABPH)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "pH (lab)") +
  theme_pubclean()
pH_biweekly

pH_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = LABPH)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "pH (lab)") +
  theme_pubclean()
pH_monthly

# density distribution 
pH_all <- bind_rows(loch_o_chem %>%
  select(LABPH, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(LABPH) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(LABPH) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 

# plot all samples 
pH_density <- ggplot(pH_all, aes(LABPH, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="pH",
       title="All samples")
pH_density

# plot by month
pH_density_monthly <- ggplot(pH_all, aes(LABPH, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="pH",
       title="All samples")
pH_density_monthly

# ridgelines 
pH_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = LABPH, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="pH",
       y="Year",
       title="Year-round samples")
pH_ridgeline

pH_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = LABPH, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="pH",
       y="Year",
       title="Summer only (July-August)")
pH_ridgeline_summer

# fall only 

# winter only 

# summer only 

# CONDUCTIVITY -------------------------------------------------------
# full record 
  # this is also lab rather than field. 
cond_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = LABCOND)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Conductivity (lab)") +
  theme_pubclean()
cond_full

# biweekly 
cond_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = LABCOND)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Conductivity (lab)") +
  theme_pubclean()
cond_biweekly

cond_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = LABCOND)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Conductivity (lab)") +
  theme_pubclean()
cond_monthly 

# density distributions 
# combine all into df
lcond_all <- bind_rows(loch_o_chem %>%
  select(LABCOND, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(LABCOND) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(LABCOND) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 

# density dist for all samples  
cond_density <- ggplot(lcond_all, aes(LABCOND, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="Conductivity",
       title="All samples")
cond_density

# monthly density dist
cond_density_monthly <- ggplot(lcond_all, aes(LABCOND, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="Conductivity",
       title="All samples")
cond_density_monthly

# ridgelines 
cond_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = LABCOND, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Conductivity",
       y="Year",
       title="Year-round samples")
cond_ridgeline

# summer only 
cond_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = LABCOND, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Conductivity",
       y="Year",
       title="Summer only (July-August)")
cond_ridgeline_summer

# fall only 

# winter only 

# spring only 

# NITRATE -------------------------------------------------------------
# full record 
nitrate_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") +
  labs(x = "Year", y = "Nitrate") +
  theme_pubclean()
nitrate_full

# biweekly 
nitrate_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Nitrate") +
  theme_pubclean()
nitrate_biweekly

# monthly 
nitrate_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = NO3_calc)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "Nitrate") +
  theme_pubclean()
nitrate_monthly 

# density distributions
NO3_all <- bind_rows(loch_o_chem %>%
  select(NO3, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(NO3) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(NO3) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 

# density dist for all samples 
nitrate_density <- ggplot(NO3_all, aes(NO3, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="Nitrate",
       title="All samples")
nitrate_density 

# density dist by month 
nitrate_density_monthly <- ggplot(NO3_all, aes(NO3, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="Nitrate",
       title="All samples")
nitrate_density_monthly 

# ridgelines 
# whole year 
nitrate_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = NO3_calc, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Nitrate",
       y="Year",
       title="Year-round samples")
nitrate_ridgeline

# summer only 
nitrate_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = NO3_calc, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="Nitrate",
       y="Year",
       title="Summer only (July-August)")
nitrate_ridgeline_summer

# fall only 

# winter only 

# spring only 

# TDN --------------------------------------------------------------------
# full record 
tdn_full <- ggplot(data = loch_o_chem, aes(x = DATE, y = TDN)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") +
  labs(x = "Year", y = "TDN") +
  theme_pubclean()
tdn_full

# biweekly 
tdn_biweekly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE), 
       aes(x = DATE, y = TDN)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "TDN") +
  theme_pubclean()
tdn_biweekly 

# monthly 
tdn_monthly <- ggplot(data = loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE), 
       aes(x = DATE, y = TDN)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", colour = "deeppink") +
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4, color = "deeppink") + 
  labs(x = "Year", y = "TDN") +
  theme_pubclean()
tdn_monthly

# density distribution 
  # combine all into new df
tdn_all <- bind_rows(loch_o_chem %>%
  select(TDN, MONTH, YEAR) %>%
  mutate(frequency = "weekly"),  
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  select(TDN) %>%
  mutate(frequency = "bi-monthly"),
loch_o_chem %>%
  group_by(MONTH, YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  select(TDN) %>%
  mutate(frequency = "monthly")) %>%
  #Arrange the frequency levels from most to least frequent
  mutate(frequency = factor(frequency,
                            levels=c("weekly",
                                     "bi-monthly",
                                     "monthly"))) 

# density dist 
tdn_density <- ggplot(tdn_all, aes(TDN, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  labs(x="TDN",
       title="All samples")
tdn_density

# density dist by month 
tdn_density_monthly <- ggplot(tdn_all, aes(TDN, color=frequency, fill=frequency))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="TDN",
       title="All samples")
tdn_density_monthly

# ridgelines 
# whole year 
tdn_ridgeline <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" & TYPE=="NORMAL"), aes(x = TDN, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#c26f8b",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="TDN",
       y="Year",
       title="Year-round samples")
tdn_ridgeline

# summer only 
tdn_ridgeline_summer <- ggplot(water_chem %>%
         filter(SITE=="LOCH.O" &
                  TYPE=="NORMAL" &
                  MONTH %in% c(7,8)), aes(x = TDN, y = YEAR, group=YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, #You can comment this out to keep the long tails
    fill = "#b2df8a",
    alpha = 0.4)+
  scale_y_reverse(breaks = c(seq(2022, 1982, -4)))+
  theme_minimal()+
  labs(x="TDN",
       y="Year",
       title="Summer only (July-August)")
tdn_ridgeline_summer

# fall only 

# winter only 

# spring only 



























