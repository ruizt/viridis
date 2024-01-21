library(tidyverse)
library(lubridate)

## data preprocessing

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


tsel %>% 
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id) 

# upper bound for date range
tsel %>%
  filter(original.id == 'NG2') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# lower bound for date range
tsel %>%
  filter(original.id == 'G5', location == 'field') %>%
  arrange(desc(datetime)) %>%
  transmute(datetime = datetime, day = yday(datetime))

# inspect proposed subset
tsel %>%
  filter(yday(datetime) <= 205, yday(datetime) >= 182) %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '')

# convert dates to hour and day and extract variables of interest
snakes <- tsel %>%
  filter(yday(datetime) <= 205, 
         yday(datetime) >= 182) %>%
  mutate(day = yday(datetime),
         hour = hour(datetime)) %>%
  select(id, type, datetime, day, hour, temp)

# save
save(snakes, file = 'data/snakes.RData')
