## Here is a basic skeleton script for running the linear model

#Pull in some actual LVWS data to work with
source("scripts/04_analysis_intermediate.R")


linear_mod_all_data <- loch_o_chem %>%
  select(DATE, MONTH, CA:SO4, PO4:MN, DOC:NO3_calc) %>% #select only the columns we want to analyze. 
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
                                TRUE ~"NA")) #Add asterisks to denote trend significant strength

# Then you will do this for the other two subsamples datasets.
# To make the tables, I like the huxtable library
# You'll basically take your model output dataframes, bind them together,
# then filter out the solute you're interested in.
# DM me if you have questions! 
