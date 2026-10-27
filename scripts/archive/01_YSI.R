source("scripts/00_libraries.R")

# file paths need to be updated - this should be done -AGK

# this will throw error messages - it's okay. 

# separate script for plotting? separate script for export? 

# LOCH ---------------------------------------------------------------
loch_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/LOCH/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

# LOCH OUTLET ------------------------------------------------------
locho_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/LOCH_OUTLET/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

# FERN LAKE ------------------------------------------------------
fern_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/FERN_LAKE/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

# SKY POND  -------------------------------------------------------
sky_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/SKY_POND/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

# SKY INLET -------------------------------------------------------
sky_inlet_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/SKY_INLET/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))

# SKY OUTLET -------------------------------------------------------
sky_outlet_ysi <- bind_rows(fs::dir_ls("Data/LVWS/01_YSI/SKY_OUTLET/raw", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))







 
#plot all variables at once  

ggplot(data = loch_long, aes(x = value, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(parameter~date, scales="free_x")

#plot temperature 
ggplot(data = loch_ysi, aes(x = temp, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot conductivity 
ggplot(data = loch_ysi, aes(x = cond, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot %DO 
ggplot(data = loch_ysi, aes(x = do_per, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot %DO 
ggplot(data = loch_ysi, aes(x = do_per, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot DO mg/L 
ggplot(data = loch_ysi, aes(x = do_mg, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot DO mg/L 
ggplot(data = loch_ysi, aes(x = do_mg, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)

#plot orp = oxidation-reduction potential 
ggplot(data = loch_ysi, aes(x = orp, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)


#plot pH
ggplot(data = loch_ysi, aes(x = pH, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()+
  facet_wrap(~date)











