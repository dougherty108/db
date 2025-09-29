source("functions/03_miniDOT.R")
source("functions/04_HOBO.R")


main_dir <-  here("data/sensors/HOBO")
all_HOBO <- compile_HOBO_data(filepath = main_dir)


SJ_HOBO <- all_HOBO %>%
  filter(lake_ID %in% c("UFM","LFM","TUC")) %>%
  select(date_time, lake_ID, depth_from_top, temperature_C) %>%
  pivot_longer(temperature_C)

SJ_DO <- combined_data_clean %>%
  filter(lake_id %in% c("UFM","LFM","TUC")) %>%
  rename(temperature_C=temp,
         lake_ID=lake_id) %>%
  ungroup() %>%
  select(date_time, temperature_C, do_obs, lake_ID, depth_from_top) %>%
  pivot_longer(-c(date_time, lake_ID, depth_from_top))

head(SJ_HOBO)
head(SJ_DO)

SJ_buoy <- bind_rows(SJ_DO, SJ_HOBO) %>%
  mutate(date=date(date_time),
         depth_from_top=as.factor(depth_from_top))

SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="temperature_C") %>%
  filter(!depth_from_top == "1.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2024-07-19") & date < as.Date("2024-09-06") | 
           date >= as.Date("2024-09-08") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date")+
  theme_minimal()+
  scale_color_colorblind()

# Freeze?
SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="temperature_C") %>%
  filter(!depth_from_top == "1.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2024-10-01") & date < as.Date("2024-12-01")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date")+
  theme_minimal()+
  scale_color_colorblind()
