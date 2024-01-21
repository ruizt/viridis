library(tidyverse)
library(lubridate)
# setwd("~/Documents/projects/2020-2025/moniz")

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

id_conversion <- tsel %>%
  select(contains('id')) %>%
  distinct()

# temp profiles by snake
tsel %>% 
  ggplot(aes(x = datetime, 
             y = temp, 
             color = location)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~original.id)

ggsave(filename = 'img/temp-by-snake.png', width = 12, height = 10, units = 'in')


# hour of day temp profiles by snake
tsel %>%
  arrange(id, datetime) %>%
  ggplot(aes(x = hour(datetime), 
             y = temp,
             color = location,
             group = yday(datetime))) +
  geom_path(alpha = 0.1) +
  facet_wrap(~original.id)

ggsave(filename = 'img/temp-by-snake-daily.png', width = 12, height = 10, units = 'in')

# daily average temp profiles by snake
tsel %>% 
  group_by(day = yday(datetime), id, type, location) %>%
  dplyr::summarize(temp = mean(temp)) %>%
  ggplot(aes(x = day, 
             y = temp, 
             group = id, 
             color = type)) + 
  geom_path(alpha = 0.5)

ggsave(filename = 'img/average-temp-profile.png', width = 8, height = 4, units = 'in')


# preferred temp ranges
Glow <- 29.1
Ghigh <- 31.9
NGlow <- 30.6
NGhigh <- 33.45

tpref <- expand_grid(
  location = c('rook', 'field'),
  type = c('tpref.min', 'tpref.max')
  ) %>%
  mutate(value = c(Glow, Ghigh, NGlow, NGhigh)) %>%
  pivot_wider(names_from = type, values_from = value)

# augment data with calculated deviations from preferred temp
tsel_aug <- left_join(tsel, tpref) %>%
  mutate(value.min = temp - tpref.min,
         value.max = temp - tpref.max,
         de = if_else(sign(value.min) == sign(value.max), 
                      if_else(sign(value.min) == 1,
                              value.max,
                              value.min),
                      0))

# thermoregulatory efficiency profiles by snake
tsel_aug %>%
  ggplot(aes(x = datetime, 
             y = de, 
             group = id, 
             color = location)) + 
  geom_path(alpha = 0.2) +
  facet_wrap(~id)

ggsave(filename = 'img/efficiency-by-snake.png', width = 12, height = 10, units = 'in')


# plotting artifact, confirm same frequency
tsel_aug %>%
  filter(id < 3) %>%
  group_by(id) %>%
  slice_head(n = 300) %>%
  ggplot(aes(x = datetime, 
             y = de, 
             group = id, 
             color = location)) + 
  geom_path(alpha = 0.6) +
  facet_wrap(~id)

# daily averages by snake
tsel_aug %>%
  group_by(day = yday(datetime), id, type) %>%
  dplyr::summarize(de = mean(de)) %>%
  ggplot(aes(x = day, 
             y = de, 
             group = id, 
             color = type)) + 
  geom_path(alpha = 0.5)

ggsave(filename = 'img/average-efficiency-profiles.png', width = 8, height = 4, units = 'in')

# density estimates for de
tsel_aug %>%
  ggplot(aes(x = de)) +
  geom_density(bw = 0.7)

tsel_aug %>%
  ggplot(aes(x = de, color = type, fill = type)) +
  geom_density(bw = 0.7, alpha = 0.3)

ggsave(filename = 'img/efficiency-distributions.png', width = 8, height = 4, units = 'in')

# estimated daily mean efficiencies by type
tsel_aug %>%
  group_by(type, date = date(datetime)) %>%
  dplyr::summarize(de = mean(de)) %>%
  ggplot(aes(x = date, y = de, color = type)) +
  geom_path()

ggsave(filename = 'img/average-daily-efficiency-by-type.png', width = 8, height = 4, units = 'in')

# difference in estimates (g - ng)
tsel_aug %>%
  group_by(type, date = date(datetime)) %>%
  dplyr::summarize(de = mean(de)) %>%
  pivot_wider(names_from = type, values_from = de) %>%
  mutate(de.diff = G - N) %>%
  ggplot(aes(x = date, y = de.diff)) +
  geom_path()

ggsave(filename = 'img/difference-in-daily-averages.png', width = 8, height = 4, units = 'in')


# same, if data are not aggregated by day
tsel_aug %>%
  group_by(type, datetime) %>%
  dplyr::summarize(de = mean(de)) %>%
  ggplot(aes(x = datetime, y = de, color = type)) +
  geom_path(alpha = 0.5)

ggsave(filename = 'img/average-efficiency-by-type.png', width = 8, height = 4, units = 'in')

tsel_aug %>%
  mutate(datetime = round_date(datetime, unit = 'hour')) %>%
  group_by(type, datetime)  %>%
  dplyr::summarize(de = mean(de, na.rm = T)) %>%
  pivot_wider(names_from = type, values_from = de) %>%
  mutate(de.diff = G - N) %>%
  ggplot(aes(x = datetime, y = de.diff)) +
  geom_path()

ggsave(filename = 'img/difference-in-averages.png', width = 8, height = 4, units = 'in')

# 'zoom in'
tsel_aug %>%
  group_by(type, datetime) %>%
  dplyr::summarize(de = mean(de)) %>%
  slice_max(order_by = datetime, n = 1000) %>%
  ggplot(aes(x = datetime, y = de, color = type)) +
  geom_path(alpha = 0.5)

ggsave(filename = 'img/average-efficiency-by-type-zoom.png', width = 8, height = 4, units = 'in')

tsel_aug %>%
  mutate(datetime = round_date(datetime, unit = 'hour')) %>%
  group_by(type, datetime)  %>%
  dplyr::summarize(de = mean(de, na.rm = T)) %>%
  slice_max(order_by = datetime, n = 1000) %>%
  pivot_wider(names_from = type, values_from = de) %>%
  mutate(de.diff = G - N) %>%
  ggplot(aes(x = datetime, y = de.diff)) +
  geom_path()

ggsave(filename = 'img/difference-in-averages-zoom.png', width = 8, height = 4, units = 'in')

# difference in hourly averages by time of day
tsel_aug %>%
  mutate(datetime = round_date(datetime, unit = 'hour')) %>%
  group_by(type, datetime)  %>%
  dplyr::summarize(de = mean(de, na.rm = T)) %>%
  pivot_wider(names_from = type, values_from = de) %>%
  mutate(de.diff = G - N) %>%
  ggplot(aes(x = hour(datetime), y = de.diff, group = date(datetime))) +
  geom_path(alpha = 0.1)

ggsave(filename = 'img/difference-by-time-of-day.png', width = 8, height = 4, units = 'in')


#### FDA SCRATCH WORK
library(fda)
library(perm)

# compute daily average de and pivot wide
de_data_wide <- tsel_aug %>%
  dplyr::select(original.id, datetime, de) %>%
  pivot_wider(id_cols = datetime, 
              values_from = de, 
              names_from = original.id) %>%
  drop_na() %>%
  group_by(t = yday(datetime)) %>%
  summarize(across(-datetime, mean))

# extract data matrix
fda_in <- de_data_wide %>%
  dplyr::select(-t) %>%
  as.matrix()

# coerce to functional object
t_range <- pull(de_data_wide, t)
basis <- create.bspline.basis(rangeval = range(t_range), 
                              nbasis = 89)
de_data_fd <- Data2fd(argvals = t_range,
                      y = fda_in, 
                      basisobj = basis)

# plot smoothed curves
types <- str_sub(colnames(fda_in),1,1) %>% factor() %>% as.numeric()
plot.fd(de_data_fd, col = types, nx = 10000)

# compute functional pcs
pca_out <- pca.fd(de_data_fd, nharm = 6, centerfns = T)
pca_out$varprop # 80% first component

de_data_fd$coefs %>% dim()
eval.basis(t_range, basisobj = basis) %>% dim()

eval.basis(t_range, basis) %*% de_data_fd$coefs %>%
  as_tibble() %>%
  mutate(t = row_number()) %>%
  pivot_longer(cols = -t) %>%
  ggplot(aes(x = t, 
             y = value, 
             group = name, 
             color = str_sub(name, 1, 1))) +
  geom_path(alpha = 0.5)

eval.basis(t_range, basis) %*% (pca_out$harmonics$coefs[, 1]*(de_data_fd$coefs)) %>%
  as_tibble() %>%
  mutate(t = row_number()) %>%
  pivot_longer(cols = -t) %>%
  ggplot(aes(x = t, 
             y = value, 
             group = name, 
             color = str_sub(name, 1, 1))) +
  geom_path(alpha = 0.5)

# extract first component
fpc_data <- tibble(snakeid = colnames(fda_in),
                   fpc = pca_out$scores[,1]) %>%
  mutate(type = str_sub(snakeid, 1, 1))

# permutation test
permTS(fpc ~ type, data = fpc_data)

# try without aggregation to daily averages
de_data_wide <- tsel_aug %>%
  dplyr::select(original.id, datetime, de) %>%
  pivot_wider(id_cols = datetime, 
              values_from = de, 
              names_from = original.id) %>%
  drop_na() %>%
  arrange(datetime) %>%
  mutate(t = row_number()) %>%
  dplyr::select(-datetime)

fda_in <- de_data_wide %>%
  dplyr::select(-t) %>%
  as.matrix()

t_range <- pull(de_data_wide, t)
basis <- create.fourier.basis(rangeval = range(t_range), 
                              nbasis = 149)
de_data_fd <- Data2fd(argvals = t_range,
                      y = fda_in, 
                      basisobj = basis)

# plot smoothed curves
types <- str_sub(colnames(fda_in),1,1) %>% factor() %>% as.numeric()
plot.fd(de_data_fd, col = types, nx = 10000)

# compute functional pcs
pca_out <- pca.fd(de_data_fd, nharm = 6, centerfns = T)
pca_out$varprop # 80% first component

# extract first component
fpc_data <- tibble(snakeid = colnames(fda_in),
                   fpc = pca_out$scores[,1]) %>%
  mutate(type = str_sub(snakeid, 1, 1))

# permutation test
permTS(fpc ~ type, data = fpc_data)
