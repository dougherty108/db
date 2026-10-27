source("scripts/00_libraries.R")

# read in lvws inventory and check structure
lvws_inventory <- read.csv("Data/Loch Vale/loch_vale_inventory.csv") %>%
  rename(lake = 1, site_depth = 2, sample_type = 3, treatment = 4, date = 5, 
         notes = 7) %>%
  select(1:5, 7) %>%
  mutate(sample_type = as.factor(sample_type)) %>%
  mutate(treatment = as.factor(treatment))
str(lvws_inventory)

# filter for doc/tdn samples
lvws_doc_tdn <- lvws_inventory %>%
  filter(sample_type == "DOC/TDN") %>%
  select(1:3, 5, 6) 

# write to csv
write_csv(lvws_doc_tdn, "Data/Loch Vale/lvws_doctdn_inventory.csv", col_names = TRUE)

lvws_shimadzu <- lvws_doc_tdn %>%
  filter(grepl('Shimadzu', notes))

write_csv(lvws_shimadzu, "Data/Loch Vale/lvws_shimadzu_inventory.csv", col_names = TRUE)

# filter for filtered_untreated, total p, and subset of doc/tdn to send to Tim Fegel
lvws_tormrs <- lvws_inventory %>%
  filter(sample_type == "DOC/TDN" | sample_type == "major ions" | sample_type == "TP")

# subsample for each sample type to count
lvws_tormrs %>% filter(sample_type == "DOC/TDN")
lvws_tormrs %>% filter(sample_type == "major ions")
lvws_tormrs %>% filter(sample_type == "TP")

# write to csv
write_csv(lvws_tormrs, "Data/Loch Vale/lvws_tormrs_inventory.csv", col_names = TRUE)















