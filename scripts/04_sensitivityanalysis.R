# creating new script for figures, etc. for abridged lvws sensitivity analysis/mtnclimn talk -AGK 240827

# source library and intermediate script -------------------------------------
source("scripts/00_libraries.R")
source("scripts/04_analysis_intermediate.R")
# this generates more than what is needed but isn't a priority for being cleaned right now. 
  # what's important is that loch_o_chem comes from this

# build and modify df --------------------------------------------------------
# convert year to water year 
loch_o_chem$WATER_YEAR <- calcWaterYear(loch_o_chem$DATE)

# convert date to day of year for the sake of parsing by season 
loch_o_chem$DAY_YEAR <- yday(loch_o_chem$DATE)

# converting to season 
# apr 15 - jun 15 = spring (peak snowmelt)
# july 15 - sep 15 = baseflow
# nov 15 - apr 15 = winter (ice reliably on lake, low flow)

# group by water year and parse seasons (spring, baseflow, and winter)

# this needs to be written to a separate dataframe rather than loch_o_chem 
  # such that variables can still be plotted monthly 

# curious if I can do this after subsampling/building second df? 

# create seasonal dataframe 
  # the issue with this is that it does not include subsampled dataframes. 
loch_o_chem_season <- loch_o_chem %>%
  group_by(WATER_YEAR) %>%
  mutate(season = case_when(
    DAY_YEAR >= 105 & DAY_YEAR <= 166 ~ "Spring", 
    DAY_YEAR >= 196 & DAY_YEAR <= 258 ~ "Baseflow", 
    DAY_YEAR >= 319 & DAY_YEAR <= 365 | DAY_YEAR >= 0 & DAY_YEAR < 105 ~ "Winter")) %>%
  filter(!is.na(season))

# build subsampled dataframe 
  # three intermediate dataframes (weekly/full record, biweekly, and monthly), specify sampling freq
Weekly <- loch_o_chem %>%
  mutate(dataset_id = "Weekly")

Biweekly <- loch_o_chem %>% 
  group_by(MONTH, WATER_YEAR) %>%
  sample_n(size = 2, replace = FALSE) %>%
  mutate(dataset_id = "Biweekly")

Monthly <- loch_o_chem %>% 
  group_by(MONTH, WATER_YEAR) %>%
  sample_n(size = 1, replace = FALSE) %>%
  mutate(dataset_id = "Monthly")

# combine subsampled dfs into full df 
# convert dataset_id (sampling frequency) to a factor, rearrange so they plot in order
  # this is not seasonally grouped! 
  # this is used for plotting when grouped by month. 
full_data <- bind_rows(Weekly, Biweekly, Monthly) %>%
  mutate(dataset_id = factor(dataset_id,
                         levels=c("Weekly","Biweekly","Monthly"))) 

# create seasonally grouped df
  # this is used for plotting when grouped by season. 
data_seasons <- full_data %>%
  group_by(WATER_YEAR) %>%
  mutate(season = case_when(
    DAY_YEAR >= 105 & DAY_YEAR <= 166 ~ "Spring", 
    DAY_YEAR >= 196 & DAY_YEAR <= 258 ~ "Baseflow", 
    DAY_YEAR >= 319 & DAY_YEAR <= 365 | DAY_YEAR >= 0 & DAY_YEAR < 105 ~ "Winter")) %>%
  filter(!is.na(season)) %>%
  mutate(dataset_id = factor(dataset_id,
                         levels=c("Weekly","Biweekly","Monthly"))) %>%
  mutate(season = factor(season, 
                         levels = c("Winter", "Spring", "Baseflow")))

# set colors for plotting
datasetCols <- c("deepskyblue3","darkorange2","violetred") 

# create month labels for plotting by month 
month_labels <- c("1" = "January", "2" = "February", "3" = "March", "4" = "April", 
                  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                  "9" = "September", "10" = "October", "11" = "November", "12" = "December")

# plot temperature ----------------------------------------------------------
# plot temperature by season with subsampling on same plot 
temp_seasons <- ggplot(data = full_data, aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")), #slope rather than r^2
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+ #Fix x-axis labels so consistent across panels
  labs(x = "Year", y = "Water temperature (°C)") +
  scale_y_continuous(limits = c(0, 16)) +
  theme_pubclean() + 
  theme(legend.position = "bottom") 
temp_seasons

# nitrate
nitrate_linearplot <- ggplot(data = full_data, aes(x = DATE, y = NO3_calc, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")), #slope rather than r^2
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+ #Fix x-axis labels so consistent across panels
  labs(x = "Year", y = "Nitrate") +
  #scale_y_continuous(limits = c(0, 16)) +
  theme_pubclean() + 
  theme(legend.position = "bottom") 
nitrate_linearplot

# plot nitrate by season 
nitrate_seasons <- ggplot(data = data_seasons, aes(x = DATE, y = NO3_calc, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")), #slope rather than r^2
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+ #Fix x-axis labels so consistent across panels
  labs(x = "Year", y = "Nitrate") +
  #scale_y_continuous(limits = c(0, 16)) +
  theme_pubclean() + 
  theme(legend.position = "bottom") 
nitrate_seasons


# save plot 
ggsave("temp_seasons.png", plot = temp_seasons, width = 14, height = 8, units = "in")

# plot each frequency as a separate layer 
# weekly 
temp_weekly <- ggplot(data = full_data %>%
                        filter(dataset_id == "weekly"), aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+#Fix x-axis labels so consistent across panels
  scale_y_continuous(limits = c(0, 16)) +
  labs(x = "Year", y = "Water temperature (°C)") +
  theme_pubclean() +
  theme(legend.position = "bottom")
temp_weekly

ggsave("temp_weekly.png", plot = temp_weekly, width = 14, height = 8, units = "in")

# weekly and biweekly 
temp_biweekly <- ggplot(data = full_data %>%
                        filter(dataset_id == "biweekly" | dataset_id == "weekly"), aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+ #Fix x-axis labels so consistent across panels
  labs(x = "Year", y = "Water temperature (°C)") +
  scale_y_continuous(limits = c(0, 16)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
temp_biweekly

ggsave("temp_biweekly.png", plot = temp_biweekly, width = 14, height = 8, units = "in")

# not sure if I need this one - will just overlay plot with all three. 
temp_monthly <- ggplot(data = full_data %>%
                        filter(dataset_id == "monthly"), aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+ #Fix x-axis labels so consistent across panels
  labs(x = "Year", y = "Water temperature (°C)") +
  theme_pubclean() +
  scale_y_continuous(limits = c(0, 16)) 
temp_monthly

# plot temp by month 
temp_monthly_linear <- ggplot(data = full_data, aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  labs(x = "Year", y = "Water temperature (ºC)") +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_monthly_linear
ggsave("temp_monthly_linear.png", plot = temp_monthly_linear, width = 14, height = 8, units = "in")

# layer weekly and biweekly 
   # note - sampling frequencies have been capitalized. make sure they're being called correctly
temp_monthly_linear_weekly <- ggplot(data = full_data %>%
                                       filter(dataset_id == "Weekly"), aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  labs(x = "Year", y = "Water temperature (ºC)") +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_monthly_linear_weekly
ggsave("temp_monthly_linear_weekly.png", plot = temp_monthly_linear_weekly, width = 14, height = 8, units = "in")

temp_monthly_linear_biweekly <- ggplot(data = full_data %>%
                                       filter(dataset_id == "Weekly" | dataset_id == "Biweekly"), aes(x = DATE, y = TEMP, group = dataset_id, colour = dataset_id, fill=dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ MONTH, scales = "free", labeller = as_labeller(month_labels)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  labs(x = "Year", y = "Water temperature (ºC)") +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_monthly_linear_biweekly
ggsave("temp_monthly_linear_biweekly.png", plot = temp_monthly_linear_biweekly, width = 14, height = 8, units = "in")

# plot ANC -------------------------------------------------------------------
# run above code but modify for ANC
# all sampling frequencies 
anc_seasons <- ggplot(data = full_data, aes(x = DATE, y = ANC, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "ANC") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 250)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_seasons

ggsave("anc_seasons.png", plot = anc_seasons, width = 14, height = 8, units = "in")

anc_weekly <- ggplot(data = full_data %>%
                       filter(dataset_id == "weekly"), aes(x = DATE, y = ANC, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "ANC") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 250)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_weekly

ggsave("anc_weekly.png", plot = anc_weekly, width = 14, height = 8, units = "in")

anc_biweekly <- ggplot(data = full_data %>%
                       filter(dataset_id == "weekly" | dataset_id == "biweekly"), aes(x = DATE, y = ANC, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "ANC") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 250)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_biweekly

ggsave("anc_biweekly.png", plot = anc_biweekly, width = 14, height = 8, units = "in")

# plot calcium -----------------------------------------------------------
# calcium
ca_seasons <- ggplot(data = data_seasons, aes(x = DATE, y = CA, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "Calcium") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 3.5)) +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_seasons
ggsave("ca_seasons.png", plot = ca_seasons, width = 14, height = 8, units = "in")

ca_weekly <- ggplot(data = data_seasons %>%
                        filter(dataset_id == "Weekly"), aes(x = DATE, y = CA, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "Calcium") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 3.5)) +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_weekly

ggsave("ca_weekly.png", plot = ca_weekly, width = 14, height = 8, units = "in")

ca_biweekly <- ggplot(data = data_seasons %>%
                        filter(dataset_id == "Weekly" | dataset_id == "Biweekly"), aes(x = DATE, y = CA, group = dataset_id, colour = dataset_id)) +
  geom_point() + 
  #geom_line() + 
  facet_wrap(~ season, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols)+
  #geom_smooth(method = "gam", colour = "purple") +
          stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
                       p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc = 0.4) + 
  labs(x = "Year", y = "Calcium") +
  scale_x_date(limits= as.Date(c("2000-01-01", "2020-01-01"), expand = c(0,0)))+
  scale_y_continuous(limits = c(0, 3.5)) +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_biweekly

ggsave("ca_biweekly.png", plot = ca_biweekly, width = 14, height = 8, units = "in")

# density plots -----------------------------------------------------------
# fix y axis 

# curious about how these would look layered w frequency (similar to above)

# update this with correct dataframe to run on seasons 
temp_density <- ggplot(full_data, aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
temp_density
ggsave("temp_density.png", plot = temp_density, width = 14, height = 8, units = "in")

temp_density_weekly <- ggplot(full_data %>%
                         filter(dataset_id == "weekly"), aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean(base_size = 18) +
  theme(legend.position = "bottom")
temp_density_weekly
ggsave("temp_density_weekly.png", plot = temp_density_weekly, width = 14, height = 8, units = "in")

temp_density_biweekly <- ggplot(full_data %>%
                         filter(dataset_id == "weekly" | dataset_id == "biweekly"), aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
temp_density_biweekly
ggsave("temp_density_biweekly.png", plot = temp_density_biweekly, width = 14, height = 8, units = "in")

# plot temp density by month to match other figures 
  # plot by layer as well
  # need to relabel months
temp_density_bymonth <- ggplot(full_data, aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y", labeller = as_labeller(month_labels))+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_density_bymonth
ggsave("temp_density_bymonth.png", plot = temp_density_bymonth, width = 14, height = 8, units = "in")

temp_density_bymonth_weekly <- ggplot(full_data %>%
                                        filter(dataset_id == "Weekly"), aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y", labeller = as_labeller(month_labels))+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_density_bymonth_weekly
ggsave("temp_density_bymonth_weekly.png", plot = temp_density_bymonth_weekly, width = 14, height = 8, units = "in")

temp_density_bymonth_biweekly <- ggplot(full_data %>%
                                        filter(dataset_id == "Weekly" | dataset_id == "Biweekly"), aes(TEMP, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y", labeller = as_labeller(month_labels))+
  labs(x="Water temperature (ºC)")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
temp_density_bymonth_biweekly
ggsave("temp_density_bymonth_biweekly.png", plot = temp_density_bymonth_biweekly, width = 14, height = 8, units = "in")


anc_density <- ggplot(full_data, aes(ANC, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="ANC") +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_density
ggsave("anc_density.png", plot = anc_density, width = 14, height = 8, units = "in")

anc_density_weekly <- ggplot(full_data %>%
                        filter(dataset_id == "weekly"), aes(ANC, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="ANC") +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_density_weekly
ggsave("anc_density_weekly.png", plot = anc_density_weekly, width = 14, height = 8, units = "in")

anc_density_biweekly <- ggplot(full_data %>%
                        filter(dataset_id == "weekly" | dataset_id == "biweekly"), aes(ANC, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="ANC") +
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
anc_density_biweekly
ggsave("anc_density_biweekly.png", plot = anc_density_biweekly, width = 14, height = 8, units = "in")

ca_density <- ggplot(data_seasons, aes(CA, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Calcium") +
    scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_density
ggsave("ca_density.png", plot = ca_density, width = 14, height = 8, units = "in")

ca_density_weekly <- ggplot(data_seasons %>%
                              filter(dataset_id == "Weekly"), aes(CA, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Calcium") +
    scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_density_weekly
ggsave("ca_density_weekly.png", plot = ca_density_weekly, width = 14, height = 8, units = "in")

ca_density_biweekly <- ggplot(data_seasons %>%
                                filter(dataset_id == "Weekly" | dataset_id == "Biweekly"), aes(CA, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Calcium") +
    scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
ca_density_biweekly
ggsave("ca_density_biweekly.png", plot = ca_density_biweekly, width = 14, height = 8, units = "in")

nitrate_density <- ggplot(full_data, aes(NO3_calc, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~MONTH, scales="free_y")+
  labs(x="Nitrate")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom")
nitrate_density
ggsave("ca_density_biweekly.png", plot = ca_density_biweekly, width = 14, height = 8, units = "in")

nitrate_density_seasonally <- ggplot(data_seasons, aes(NO3_calc, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Nitrate")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
nitrate_density_seasonally
ggsave("nitrate_density_seasonally.png", plot = nitrate_density_seasonally, width = 14, height = 8, units = "in")

nitrate_density_weekly <- ggplot(data_seasons %>%
                                   filter(dataset_id == "Weekly"), aes(NO3_calc, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Nitrate")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
nitrate_density_weekly
ggsave("nitrate_density_weekly.png", plot = nitrate_density_weekly, width = 14, height = 8, units = "in")

nitrate_density_biweekly <- ggplot(data_seasons %>%
                                   filter(dataset_id == "Weekly" | dataset_id == "Biweekly"), aes(NO3_calc, color=dataset_id, fill=dataset_id))+
    geom_density(alpha=0.2)+
  facet_wrap(~season, scales="free_y")+
  labs(x="Nitrate")+ 
  scale_color_manual(values=datasetCols)+
  scale_fill_manual(values=datasetCols) +
  #scale_y_continuous(limits = c(0,1)) +
  theme_pubclean() +
  theme(legend.position = "bottom", legend.title = element_blank())
nitrate_density_biweekly
ggsave("nitrate_density_biweekly.png", plot = nitrate_density_biweekly, width = 14, height = 8, units = "in")





# fix water year - data retrieval, water year function 
# group_by water year 
# season = case_when date between values 
   # run linear models through seasons 

# updated 240911 - only temp, ANC, and calcium (?) or another analyte that increases in the winter 

# ridgeline plots --------------------------------------------------------
ggplot(water_chem %>%
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

nitrate_ridgeline <- ggplot(data = full_data, aes(x = NO3_calc, y = YEAR, group = YEAR)) +
  geom_density_ridges(
    scale = 7,
    size = 0.25,
    rel_min_height = 0.01, 
    alpha = 0.4, 
    fill = "deepskyblue3")+
  scale_y_reverse(breaks = c(seq(2020, 1995, -5)))+
  theme_minimal()+
  labs(x="Nitrate",
       y="Year") +
  theme_pubclean(base_size = 18)
nitrate_ridgeline
ggsave("nitrate_ridgeline.png", plot = nitrate_ridgeline, width = 12, height = 8, units = "in")














