source("scripts/00_libraries.R")

#build ysi database ---------------------------------------------------
loch_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI/loch", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))
#can add more columns or otherwise mutate as desired. 

locho_ysi <- bind_rows(fs::dir_ls("Data/On Thin Ice/YSI/loch_outlet", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 2, 5, 6, 8:10, 15, 17)) %>%
      dplyr::rename(date = 1, time = 2, cond = 3, depth = 4, do_per = 5, do_mg = 6, 
                    orp = 7, pH = 8, temp = 9) %>%
      mutate(date = mdy(`date`))
 
#process db ---------------------------------------------------
#pivot to long format for funsies
loch_long <- loch_ysi %>% 
  pivot_longer(cols = "cond":"temp", names_to = "parameter")

locho_long <- locho_ysi %>% 
  pivot_longer(cols = "cond":"temp", names_to = "parameter")

#plot ------------------------------------------------------
ggplot(data = loch_ysi, aes(x = temp, y = depth)) + 
  geom_point() + 
  geom_path() +
  scale_y_reverse()












