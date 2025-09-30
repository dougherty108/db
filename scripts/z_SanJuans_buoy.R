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


# TUC ---------------------------------------------------------------------


SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="temperature_C") %>%
  filter(depth_from_top == "8.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2024-07-19") & date < as.Date("2024-09-06") | 
           date >= as.Date("2024-09-08") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date",
       title="Turkey Creek Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/temperature_TUC_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)

# Freeze?
SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="temperature_C") %>%
  filter(!depth_from_top == "1.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2024-10-10") & date < as.Date("2024-11-15")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date",
       title="Turkey Creek Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/temperature_TUC_2024-2025_iceon.png",
       width = 4, height = 4, units = "in", dpi = 300)


# Thaw
SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="temperature_C") %>%
  filter(!depth_from_top == "1.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2025-05-01") & date < as.Date("2025-06-15")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date",
       title="Turkey Creek Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/temperature_TUC_2024-2025_iceoff.png",
       width = 4, height = 4, units = "in", dpi = 300)

SJ_buoy %>%
  filter(lake_ID=="TUC") %>%
  filter(name=="do_obs") %>%
  # filter(!depth_from_top == "1.6") %>% #Show Charlie or Katie -- a bit of recon needed
  filter(date > as.Date("2024-07-19") & date < as.Date("2024-09-06") | 
           date >= as.Date("2024-09-08") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Dissolved oxygen (mg/L)",
       x="Date",
       title="Turkey Creek Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/doObs_TUC_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)



# UFM ---------------------------------------------------------------------


SJ_buoy %>%
  filter(lake_ID=="UFM") %>%
  filter(name=="temperature_C") %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date",
       title="Upper Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/temperature_UFM_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)

SJ_buoy %>%
  filter(lake_ID=="UFM") %>%
  filter(name=="do_obs") %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Dissolved oxygen (mg/L)",
       x="Date",
       title="Upper Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/doObs_UFM_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)


SJ_buoy %>%
  filter(lake_ID=="UFM") %>%
  filter(depth_from_top %in% c("2","8.5")) %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2024-10-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Dissolved oxygen (mg/L)",
       x="Date",
       title="Upper Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind() +
  facet_wrap(~name,nrow=2, scales="free_y")
ggsave("plots/san juans buoy/doObs_temp_UFM_october_weirdness.png",
       width = 8, height = 4, units = "in", dpi = 300)


# LFM ---------------------------------------------------------------------


SJ_buoy %>%
  filter(lake_ID=="LFM") %>%
  filter(name=="temperature_C") %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Water temperature (deg C)",
       x="Date",
       title="Lower Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/temperature_LFM_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)

SJ_buoy %>%
  filter(lake_ID=="LFM") %>%
  filter(name=="do_obs") %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2025-07-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Dissolved oxygen (mg/L)",
       x="Date",
       title="Lower Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind()
ggsave("plots/san juans buoy/doObs_LFM_2024-2025.png",
       width = 8, height = 4, units = "in", dpi = 300)


SJ_buoy %>%
  filter(lake_ID=="LFM") %>%
  filter(depth_from_top %in% c("2.9","5.9")) %>%
  filter(date >= as.Date("2024-09-10") & date < as.Date("2024-10-16")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point()+
  labs(y="Dissolved oxygen (mg/L)",
       x="Date",
       title="Lower Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind() +
  facet_wrap(~name,nrow=2, scales="free_y")
ggsave("plots/san juans buoy/doObs_temp_LFM_october_weirdness.png",
       width = 8, height = 4, units = "in", dpi = 300)



SJ_buoy %>%
  filter(lake_ID=="LFM") %>%
  filter(depth_from_top %in% c("2.9","5.9")) %>%
  filter(date >= as.Date("2025-05-15") & date < as.Date("2025-07-01")) %>%
  ggplot(aes(x=date_time, y=value, color=depth_from_top))+
  geom_point(alpha=0.1)+
  geom_line(alpha=0.5)+
  labs(y="Value",
       x="Date",
       title="Lower Fourmile Lake")+
  theme_minimal()+
  scale_color_colorblind() +
  facet_wrap(~name,nrow=2, scales="free_y")
ggsave("plots/san juans buoy/doObs_temp_LFM_spring_weirdness.png",
       width = 8, height = 4, units = "in", dpi = 300)
