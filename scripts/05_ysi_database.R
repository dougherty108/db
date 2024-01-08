source("scripts/00_libraries.R")

#build ysi database ---------------------------------------------------
ysi_db <- bind_rows(fs::dir_ls("data/ysi/312", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "312", treatment = "burned") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/316", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "316", treatment = "control") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/322", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "322", treatment = "burned") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/326", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "326", treatment = "control") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/331", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "331", treatment = "burned") %>%
      mutate(date = mdy(`date`)),
    fs::dir_ls("data/ysi/332", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "332", treatment = "control") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/335", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "335", treatment = "control") %>%
      mutate(date = mdy(`date`)), 
    fs::dir_ls("data/ysi/336", regexp = "\\.csv$") %>%
      purrr::map_dfr( ~ read.csv(.x, sep = ",", header = FALSE, skip = 19, skipNul = TRUE)) %>%
      select(1, 5, 6, 9, 10, 15, 17, 19) %>%
      dplyr::rename(date = 1, cond = 2, depth = 3, do_mg = 4, 
                    orp = 5, turb = 6, pH = 7, temp = 8) %>%
      mutate(pond = "336", treatment = "burned") %>%
      mutate(date = mdy(`date`)))

#process db ---------------------------------------------------
#factors 
ysi_db$treatment <- as.factor(ysi_db$treatment)
ysi_db$pond <- as.factor(ysi_db$pond)
#filter to include only top meter, pivot to long format
ysi_1m <- ysi_db %>% 
  filter(depth <= 1 & depth > 0) %>%
  pivot_longer(cols = "cond":"temp", names_to = "param")

#wide format dataframe. can't remember what I was using this for
ysi_short <- ysi_db %>%
  mutate(date = as.factor(date)) %>%
  filter(depth <= 1 & depth > 0)

#stats ------------------------------------------------------
ysi_means_long <- ysi_1m %>%
  ungroup() %>%
  group_by(treatment, pond, param, date) %>%
  summarize(mean = mean(value, na.rm = TRUE))

ysi_means <- ysi_means_long %>%
  pivot_wider(names_from = param, values_from = mean) %>%
  ungroup() 
