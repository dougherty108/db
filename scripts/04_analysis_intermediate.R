source("scripts/00_libraries.R")

# read in lvws data ------------------------
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

# filter for loch.o and norm samples -------------------------------
loch_o_chem <- water_chem %>%
  filter(SITE == "LOCH.O" & TYPE == "NORMAL" & YEAR > 1995) %>%
  mutate(MONTH = as.factor(MONTH)) #%>%
  # mutate(YEAR = as.factor(YEAR))

NO3_compare <- water_chem %>%
  filter(TYPE == "NORMAL") %>%
  select(1:7, 21:22, 34, 47)

#creating month labels 
month_labels <- c("1" = "January", "2" = "February", "3" = "March", "4" = "April", 
                  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                  "9" = "September", "10" = "October", "11" = "November", "12" = "December")

# running linear model, producing table of output results -----------------


linear_mod_all_data <- loch_o_chem %>%
  select(DATE, MONTH, TEMP, CA:SO4, PO4:MN, DOC:NO3_calc) %>% #select only the columns we want to analyze. 
  #I'm ignoring a lot of the trailing columns because I don't think we have good coverage but feel
  #free to modify if I'm wrong
  mutate(DATE_DEC = decimal_date(DATE)) %>% #Convert date to a decimal to get some sensible model output
  select(-DATE) %>%
  pivot_longer(-c(DATE_DEC, MONTH)) %>%
  drop_na() %>% # The code won't run without dropping NAs, I think because some solutes have 
    #no data. You can get rid of this later if you select better columns than I did.
  nest(data = -c(name, MONTH)) %>%
    #This will create a 'nested' dataframe for each combination of solute * MONTH
    #run the script only up to line 18 if you want to explore what this looks like in View()
  
  #map comes from the 'purrr' package. This applies a function to each element in a vector
  #Think of everything in mutate() below as a pseduo for-loop
  mutate( 
    fit = map(data, ~lm(value ~ DATE_DEC, data = .x)), # Fits a linear model for every solute * MONTH combination
    tidy_out = map(fit, tidy) # 
  ) %>%
  #You get 4 warnings-- this is probably because there is not enough data for some of these to run a proper model
  unnest(cols = tidy_out) %>% #expand the tidy model output
  select(-fit, -data, -std.error, -statistic) %>% #gets rid of columns you no longer need
  filter(term != "(Intercept)") %>% #we are just going to look at the "DATE_DEC" term to look at slopes
  mutate(p.value=round(p.value,4), #do some rounding
         estimate=round(estimate,4),
         Significance=case_when(p.value<0.05 & p.value >0.01 ~ "*",
                                p.value<= 0.01 & p.value > 0.001 ~ "**",
                                p.value<= 0.001 ~ "***",
                                TRUE ~"N.S.")) %>% #Add asterisks to denote trend significant strength
  mutate(frequency = "weekly")

# filter by solute
ca_linear <- linear_mod_all_data %>%
  filter(name == "CA")
# okay rad. I need to build dataframes for bi-monthly and monthly sampling frequencies then 
  # bind those model outputs before filtering 

# bi-monthly sampling freq

bi_monthly <- loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 2, replace = FALSE)

# monthly sampling freq
monthly <- loch_o_chem %>%
                    group_by(MONTH, YEAR) %>%
                    sample_n(size = 1, replace = FALSE)

# i think that worked. wondering if there's a way to "fix" what samples it pulls 
  # such that results are consistent

# run lm for other sampling frequencies ------------------------------

#bimonthly
linear_mod_bi_monthly <- bi_monthly %>%
  select(DATE, MONTH, TEMP, CA:SO4, PO4:MN, DOC:NO3_calc) %>% 
  mutate(DATE_DEC = decimal_date(DATE)) %>% 
  select(-DATE) %>%
  pivot_longer(-c(DATE_DEC, MONTH)) %>%
  drop_na() %>% 
  nest(data = -c(name, MONTH)) %>%
    mutate( 
    fit = map(data, ~lm(value ~ DATE_DEC, data = .x)), 
    tidy_out = map(fit, tidy) # 
  ) %>%
  unnest(cols = tidy_out) %>% 
  select(-fit, -data, -std.error, -statistic) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.value=round(p.value,4), 
         estimate=round(estimate,4),
         Significance=case_when(p.value<0.05 & p.value >0.01 ~ "*",
                                p.value<= 0.01 & p.value > 0.001 ~ "**",
                                p.value<= 0.001 ~ "***",
                                TRUE ~"N.S.")) %>%
  mutate(frequency = "bi-monthly")

#monthly
linear_mod_monthly <- monthly %>%
  select(DATE, MONTH, TEMP, CA:SO4, PO4:MN, DOC:NO3_calc) %>% 
  mutate(DATE_DEC = decimal_date(DATE)) %>% 
  select(-DATE) %>%
  pivot_longer(-c(DATE_DEC, MONTH)) %>%
  drop_na() %>% 
  nest(data = -c(name, MONTH)) %>%
    mutate( 
    fit = map(data, ~lm(value ~ DATE_DEC, data = .x)), 
    tidy_out = map(fit, tidy) # 
  ) %>%
  unnest(cols = tidy_out) %>% 
  select(-fit, -data, -std.error, -statistic) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.value=round(p.value,4), 
         estimate=round(estimate,4),
         Significance=case_when(p.value<0.05 & p.value >0.01 ~ "*",
                                p.value<= 0.01 & p.value > 0.001 ~ "**",
                                p.value<= 0.001 ~ "***",
                                TRUE ~"N.S.")) %>%
  mutate(frequency = "monthly")

# progressively more warnings as data decrease. i'm assuming it's just because 
  # there isn't enough data to properly fit the model

# create results dataframe 

linear_results <- bind_rows(linear_mod_all_data, linear_mod_bi_monthly, 
                            linear_mod_monthly)

# filter by solute --------------------------------------
temp_linear <- linear_results %>%
  filter(name == "TEMP") %>%
  relocate(frequency, .after = MONTH)

ca_linear <- linear_results %>%
  filter(name == "CA") %>%
  relocate(frequency, .after = MONTH) 

mg_linear <- linear_results %>%
  filter(name == "MG") %>%
  relocate(frequency, .after = MONTH) 

na_linear <- linear_results %>%
  filter(name == "NA") %>%
  relocate(frequency, .after = MONTH) 

k_linear <- linear_results %>%
  filter(name == "K") %>%
  relocate(frequency, .after = MONTH) 

nh4_linear <- linear_results %>%
  filter(name == "NH4") %>%
  relocate(frequency, .after = MONTH) 

so4_linear <- linear_results %>%
  filter(name == "SO4") %>%
  relocate(frequency, .after = MONTH) 

cl_linear <- linear_results %>%
  filter(name == "CL") %>%
  relocate(frequency, .after = MONTH) 

sio2_linear <- linear_results %>%
  filter(name == "SiO2") %>%
  relocate(frequency, .after = MONTH) 

doc_linear <- linear_results %>%
  filter(name == "DOC") %>%
  relocate(frequency, .after = MONTH) 

nh4_calc_linear <- linear_results %>%
  filter(name == "NH4_calc") %>%
  relocate(frequency, .after = MONTH) 

no3_calc_linear <- linear_results %>%
  filter(name == "NO3_calc") %>%
  relocate(frequency, .after = MONTH) 
# 
# # set column names
# col_names <- c("Month", "Sampling Frequency", "Estimate", "p value", "Significance")
# 
# # convert to table using huxtable
# ca_table <- ca_linear %>%
#   select(MONTH, frequency, estimate, p.value, Significance) %>%
#   as_hux(colnames(col_names))
# 
# 













