library(tidyverse)
library(lubridate)

## Tb data preprocessing
#########################

# read in
tb_raw <- list.files(path = "data/raw/For_R_Analysis_Amended", 
                   full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows %>%
  mutate(time = as.character(time)) %>%
  unite('datetime', date:time, sep = ' ') %>%
  mutate(datetime = mdy_hms(datetime)) %>%
  mutate(datetime = round_date(datetime, unit = 'hour')) %>%
  mutate(type = str_sub(snakeid, 1, 1),
         id = as.numeric(factor(snakeid))) %>%
  rename(original.id = snakeid)

# plot
tb_raw %>% 
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id)

# lower bound determined by last date before snakes settle in locations
tb_raw %>%
  filter(original.id == 'G5', location == 'field') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# upper bound determined by last date of shortest series
tb_raw %>%
  filter(original.id == 'NG4') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# inspect proposed subset
tb_raw %>%
  filter(yday(datetime) <= 237, yday(datetime) >= 182) %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '')

# convert dates to hour and day and extract variables of interest
tb <- tb_raw %>%
  filter(yday(datetime) <= 237, 
         yday(datetime) >= 182,
         original.id != 'NG2') %>%
  mutate(day = yday(datetime),
         hour = hour(datetime)) %>%
  dplyr::select(id, type, datetime, day, hour, temp)

# export csv
write_csv(tb, file = 'data/tb.csv')

## Te data processing
######################

# read in
te_raw <- read_csv('data/raw/OTM_wden_20Nov23.csv') %>%
  rename_with(.fn = tolower) %>%
  mutate(datetime = mdy_hm(datetime)) %>%
  dplyr::select(-date, -time, -time_int)

# summaries of locations
otm_locations <- te_raw %>%
  distinct(id, site, location, treatment) 

# check for missingness
te_raw %>% group_by(id) %>% count()

# plot
te_raw %>%
  ggplot(aes(x = datetime, y = temp, color = treatment)) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.5)

# add day and hour variables
te <- te_raw %>%
  mutate(day = yday(datetime),
         hour = hour(datetime))

# export csv
write_csv(te, file = 'data/te.csv')


## gradient data processing
############################

# read in raw data and parse dates
gradient_raw <- read_csv('data/raw/Cviridis_ThermalGradient.csv') |>
  rename_with(tolower) |>
  mutate(time = as.character(time)) |>
  unite('datetime', date:time, sep = ' ') |>
  mutate(datetime = mdy_hms(datetime))

# observation summary
gradient_raw |>
  group_by(snakeid, repro, run) |>
  arrange(datetime) |>
  summarize(n.obs = n(),
            start = min(datetime),
            end = max(datetime),
            obs.freq.max = max(diff(datetime)),
            obs.freq.min = min(diff(datetime))) |>
  mutate(duration = (end - start)) |>
  mutate(duration = seconds_to_period(duration)) |>
  arrange(run) |>
  write_csv('data/gradient-metadata.csv')

gradient <- gradient_raw |>
  group_by(snakeid, repro) |>
  summarize(tb.med = median(tb),
            tb.q1 = quantile(tb, 0.25),
            tb.q3 = quantile(tb, 0.75),
            n.obs = n())

# export csv
write_csv(gradient, file = 'data/gradient.csv')

# write r binary file
save(list = c('gradient', 'tb', 'te'), file = 'data/viridis-data.RData')
