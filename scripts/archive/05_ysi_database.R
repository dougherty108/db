source("scripts/00_libraries.R")

# THIS SCRIPT IS OUTDATED - works, but has been replaced by the more functional
  # YSI_processFunction.R

# SCRIPT TO DOs 
  # need to update to reflect file structure - would it make more sense to have 
  # project-specific repositories rather than processing all of this here? 

# could keep this for Loch Vale/RG/San Juans processing and move OTI to another script/repo

# this script needs to: 
  # 1) read in raw data
  # 2) change header names & trim unneeded columns
  # 3) write csv w/ correct formatting 
  # 4) export csv
  # 5) visualize data

# does it make more sense to read these in individually, or search by file extension? 
  # this is going to be tricky for RG as there are so many sites


#build ysi database ---------------------------------------------------
loch_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI/loch", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))
#can add more columns or otherwise mutate as desired. 

loch_240206 <- loch_ysi %>%
  filter(date == "2024-02-06") %>%
  filter(time > "13:00:00")

write_csv(loch_240206, "Data/On Thin Ice/YSI/Loch_20240206.csv", col_names = TRUE)

locho_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI/loch_outlet", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

gold_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))
 
#process db ---------------------------------------------------
#pivot to long format for funsies
loch_long <- loch_ysi %>% 
  pivot_longer(cols = c(cond, do_per:temp), names_to = "parameter")

loch_long_240206 <- loch_240206 %>% 
  pivot_longer(cols = c(cond, do_per:temp), names_to = "parameter")

locho_long <- locho_ysi %>% 
  pivot_longer(cols = c(cond, do_per:temp), names_to = "parameter")

gold_long <- gold_ysi %>%
  relocate(depth, .after = time) %>%
  pivot_longer(cols = 4:9, names_to = "parameter")

#plot all variables at once  ------------------------------------------------------

ggplot(data = loch_long, aes(x = value, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(parameter~date, scales="free_x")

param <- c("cond", "do_mg", "do_per", "orp", "pH", "temp")
names(param) <- c("Conductivity", "Dissolved Oxygen (mg/L), Dissolved Oxygen (%)", 
            "ORP", "pH", "Temperature (ºC)")

labels <- c("cond" = "Conductivity", "do_mg" = "Dissolved Oxygen (mg/L)", "do_per" = "Dissolved Oxygen (%)",
            "pH" = "pH", "orp" = "ORP", "temp" = "Temperature (ºC)")

ggplot(data = loch_long_240206, aes(x = value, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~ parameter, scales="free_x", labeller = labeller(parameter = labels)) +
  labs(x = NULL, y = "Depth (m)") +
  theme_pubr()


#plot temperature ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = temp, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot conductivity ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = cond, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot %DO ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = do_per, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot %DO ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = do_per, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot DO mg/L ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = do_mg, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot DO mg/L ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = do_mg, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot orp = oxidation-reduction potential ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = orp, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot pH------------------------------------------------------
ggplot(data = loch_ysi, aes(x = pH, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


# gold lake data ------------------------------------------------
# plot gold lake data to send to tim

gold_long %>%
  ggplot(aes(x = value, y = depth)) +
  geom_point() +
  geom_path() +
  #geom_smooth() +
  facet_grid(~ parameter, scales = "free") +
  theme_pubclean() +
  scale_y_reverse()


gold_temp <- gold_ysi %>%
  ggplot(aes(x = temp, y = depth)) +
  labs(x = "Temperature (ºC)", y = "Depth (m)") +
  geom_point(colour = "deepskyblue4") +
  geom_path(colour = "deepskyblue4") +
  #geom_smooth() +
  scale_y_reverse() +
  theme_pubr()
gold_temp

gold_cond <- gold_ysi %>%
  ggplot(aes(x = cond, y = depth)) +
  labs(x = "Conductivity (µS/cm)", y = "Depth (m)") +
  geom_point(colour = "deepskyblue4") +
  geom_path(colour = "deepskyblue4") +
  #geom_smooth() +
  scale_y_reverse() +
  theme_pubr()
gold_cond

gold_domg <- gold_ysi %>%
  ggplot(aes(x = do_mg, y = depth)) +
  labs(x = "DO (mg/L)", y = "Depth (m)") +
  geom_point(colour = "deepskyblue4") +
  geom_path(colour = "deepskyblue4") +
  #geom_smooth() +
  scale_y_reverse() +
  theme_pubr()
gold_domg

gold_doper <- gold_ysi %>%
  ggplot(aes(x = do_per, y = depth)) +
  labs(x = "DO (%)", y = "Depth (m)") +
  geom_point(colour = "deepskyblue4") +
  geom_path(colour = "deepskyblue4") +
  #geom_smooth() +
  scale_y_reverse() +
  theme_pubr()
gold_doper

gold_orp <- gold_ysi %>%
  ggplot(aes(x = orp, y = depth)) +
  labs(x = "ORP", y = "Depth (m)") +
  geom_point(colour = "deepskyblue4") +
  geom_path(colour = "deepskyblue4") +
  #geom_smooth() +
  scale_y_reverse() +
  theme_pubr()
gold_orp

gold_all <- ggarrange(gold_temp, gold_cond, gold_domg, gold_doper, align = c("hv"))
gold_all


# data for Amy
# read in ysi hole 2 profile from 2/06 to send to Amy

loch_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI/loch", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

loch_2 <- read.csv("Data/On Thin Ice/YSI/loch/Loch_Hole2_20240206.csv", header = FALSE, skipNul = TRUE, 
                   fileEncoding = "UTF-8-BOM") %>%
  select(1, 2, 5, 6, 8:10, 15, 17) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))























