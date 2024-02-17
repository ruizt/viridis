library(tidyverse)
library(lubridate)

## Tb data preprocessing
#########################

# read in
tsel <- list.files(path = "data/For_R_Analysis_Amended", 
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

# plot raw data
tsel %>% 
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id)

ggsave(filename = 'img/exploratory/tb-raw.png',
       width = 12, height = 5, units = 'in')


# lower bound determined by last date before snakes settle in locations
tsel %>%
  filter(original.id == 'G5', location == 'field') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# upper bound determined by last date of shortest series
tsel %>%
  filter(original.id == 'NG4') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# inspect proposed subset
tsel %>%
  filter(yday(datetime) <= 237, yday(datetime) >= 182) %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '')

# convert dates to hour and day and extract variables of interest
snakes <- tsel %>%
  filter(yday(datetime) <= 237, 
         yday(datetime) >= 182,
         original.id != 'NG2') %>%
  mutate(day = yday(datetime),
         hour = hour(datetime)) %>%
  dplyr::select(id, type, datetime, day, hour, temp)

# save
save(snakes, file = 'data/snakes-60d.RData')


## Te data processing
######################

# read in
otm_raw <- read_csv('data/OTM_wden_20Nov23.csv') %>%
  rename_with(.fn = tolower) %>%
  mutate(datetime = mdy_hm(datetime)) %>%
  dplyr::select(-date, -time, -time_int)

# summaries of locations
otm_locations <- otm_raw %>%
  distinct(id, site, location, treatment) 

# plot raw data
otm_raw %>%
  ggplot(aes(x = datetime, y = temp, color = treatment)) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.5)
   
ggsave(filename = 'img/exploratory/otm-raw.png',
       width = 12, height = 5, units = 'in')

# check missingness
otm_raw %>% group_by(id) %>% count()

otm <- otm_raw %>%
  mutate(day = yday(datetime),
         hour = hour(datetime))

save(otm, file = 'data/otm.RData')
