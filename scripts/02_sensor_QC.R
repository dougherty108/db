source("scripts/01_compile_miniDOT.R")


# Create flag dataframes --------------------------------------------------
# Create dataframes that put the data into long format and flag outliers
# based on 3 std devs for now
sky_flag <- sky_minidot %>%
  mutate(year = year(date_time),
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs, do_sat)) %>%
  group_by(depth, year, name) %>%
  mutate(value_scale = scale(value),
         flag_3sd = case_when(abs(value_scale) > 3 ~ "yes",
                          .default = "no"),
         flag_4sd = case_when(abs(value_scale) > 4 ~ "yes",
                            .default = "no"))

loch_flag <- loch_minidot %>%
  mutate(year = year(date_time),
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs, do_sat)) %>%
  group_by(depth, year, name) %>%
  mutate(value_scale = scale(value),
         flag_3sd = case_when(abs(value_scale) > 3 ~ "yes",
                              .default = "no"),
         flag_4sd = case_when(abs(value_scale) > 4 ~ "yes",
                              .default = "no"))

fern_flag <- fern_minidot %>%
  mutate(year = year(date_time),
         month = month(date_time)) %>%
  pivot_longer(c(temp, do_obs, do_sat)) %>%
  group_by(depth, year, name) %>%
  mutate(value_scale = scale(value),
         flag_3sd = case_when(abs(value_scale) > 3 ~ "yes",
                              .default = "no"),
         flag_4sd = case_when(abs(value_scale) > 4 ~ "yes",
                              .default = "no"))


# Raw data plots ----------------------------------------------------------


#do_obs
sky_do <- ggplot(data = sky_flag_3sd %>%
                   filter(name == "do_obs") %>%
                   filter(value < 100),
                 aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_do

loch_do <-
  ggplot(data = loch_flag_3sd %>%
           filter(name == "do_obs"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_do

fern_do <-
  ggplot(data = fern_flag_3sd %>%
           filter(name == "do_obs"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
fern_do
#wondering where the rest of the surface data is?

#do sat
sky_do_sat <- ggplot(data = sky_flag_3sd %>%
                       filter(name == "do_sat"),
                     aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_do_sat

loch_do_sat <-
  ggplot(data = loch_flag_3sd %>%
           filter(name == "do_sat"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_do_sat


fern_do_sat <-
  ggplot(data = fern_flag_3sd %>%
           filter(name == "do_sat"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m-%d"
  )
fern_do_sat
#wondering where the rest of the surface data is?

#temp
sky_temp <- ggplot(data = sky_flag_3sd %>%
                       filter(name == "temp"),
                     aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_temp

loch_temp <-
  ggplot(data = loch_flag_3sd %>%
           filter(name == "temp"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_temp


fern_temp <-
  ggplot(data = fern_flag_3sd %>%
           filter(name == "temp"),
         aes(x = date_time, y = value)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap2(depth ~ year, scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m-%d"
  )
fern_temp


# z-score plots -----------------------------------------------------------


#plot z-scores for do_sat, do_obs, and temp for sky pond and loch
sky_do_scaled <- ggplot(data = sky_flag_3sd %>%
                          filter(name == "do_obs"),
                        aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_do_scaled

sky_do_sat_scaled <- ggplot(data = sky_flag_3sd %>%
                          filter(name == "do_sat"),
                        aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_do_sat_scaled

sky_temp_scaled <- ggplot(data = sky_flag_3sd %>%
                            filter(name == "temp"),
                          aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
sky_temp_scaled


loch_do_scaled <- ggplot(data = loch_flag_3sd %>%
                          filter(name == "do_obs"),
                        aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_do_scaled

loch_do_sat_scaled <- ggplot(data = loch_flag_3sd %>%
                              filter(name == "do_sat"),
                            aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_do_sat_scaled

loch_temp_scaled <- ggplot(data = loch_flag_3sd %>%
                            filter(name == "temp"),
                          aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
loch_temp_scaled

fern_do_scaled <- ggplot(data = fern_flag_3sd %>%
                          filter(name == "do_obs"),
                        aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
fern_do_scaled

fern_do_sat_scaled <- ggplot(data = fern_flag_3sd %>%
                              filter(name == "do_sat"),
                            aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
fern_do_sat_scaled

fern_temp_scaled <- ggplot(data = fern_flag_3sd %>%
                            filter(name == "temp"),
                          aes(x = date_time, y = value_scale)) +
  geom_point(aes(color = flag_3sd, shape = flag_3sd)) +
  facet_wrap(depth ~ year(date_time), scales = "free") +
  scale_x_datetime(
    date_breaks = "3 months",
    date_minor_breaks = "1 month",
    date_labels = "%m"
  )
fern_temp_scaled
