library(tidyverse)
library(modelr)
library(fda)
load('data/snakes.RData')

# raw data plots
snakes %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = type)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '')

ggsave(filename = 'img/exploratory/raw-tb-30d.png', width = 8, height = 8, units = 'in')

snakes %>%
  ggplot(aes(x = datetime, 
             y = temp,
             color = type,
             group = id)) +
  geom_path(alpha = 0.2)

ggsave(filename = 'img/exploratory/raw-tb-30d-overlaid.png', width = 12, height = 6, units = 'in')

snakes %>%
  ggplot(aes(x = hour,
             y = temp,
             color = type,
             group = interaction(day, id))) +
  geom_path(alpha = 0.05) +
  geom_smooth(formula = 'y ~ x',
              method = 'loess',
              span = 0.2,
              aes(x = hour, y = temp, color = type), 
              se = F, inherit.aes = F, alpha = 0.5)

ggsave(filename = 'img/exploratory/hourly-overlaid-loess.png', width = 8, height = 6, units = 'in')

night <- c(18:24, 0:6)
snakes %>%
  mutate(time.of.day = factor(hour %in% night, 
                              labels = c('day', 'night')),
         day.lag = lag(day, n = 7)) %>%
  mutate(day.lag = replace_na(day.lag, 181)) %>%
  group_by(id, type, day.lag, time.of.day) %>%
  summarize(temp = median(temp)) %>%
  ggplot(aes(x = day.lag, y = temp, color = type)) +
  facet_wrap(~time.of.day) +
  geom_path(aes(group = id), alpha = 0.3) +
  geom_smooth(formula = 'y ~ x', 
              method = 'loess', 
              span = 1, se = F)

ggsave(filename = 'img/exploratory/daily-medians-day-night.png', width = 10, height = 5, units = 'in')

# group averages
snakes %>%
  group_by(type, datetime, day, hour) %>%
  summarize(mean_temp = mean(temp))  %>%
  ggplot(aes(x = datetime, y = mean_temp, color = type)) +
  geom_path()

ggsave(filename = 'img/exploratory/daily-group-means.png', width = 8, height = 4, units = 'in')

snakes %>%
  group_by(type, datetime, day, hour) %>%
  summarize(mean_temp = mean(temp))  %>%
  ggplot(aes(x = hour, y = mean_temp, color = type)) +
  geom_path(aes(group = interaction(type, day)), alpha = 0.2)

ggsave(filename = 'img/exploratory/hourly-group-means.png', width = 8, height = 4, units = 'in')

## model 1: hourly categorical, no correlation

# compute fit
fit1 <- lm(temp ~ type*factor(hour), data = snakes)

fit1_df <- snakes %>%
  add_predictions(fit1) %>%
  add_residuals(fit1)

# estimated means vs observations
fit1_df %>%
  ggplot(aes(x = datetime, y = temp, color = type, group = id)) +
  geom_path(alpha = 0.2) +
  geom_path(aes(y = pred))

ggsave(filename = 'img/exploratory/lm-time-categorical-fitted-means.png', width = 8, height = 4, units = 'in')

# same, by time of day
fit1_df %>%
  ggplot(aes(x = hour, y = temp, color = type)) +
  geom_path(aes(group = interaction(id, day)), alpha = 0.05) +
  geom_path(aes(y = pred, group = interaction(day, id)))

ggsave(filename = 'img/exploratory/lm-time-categorical-fitted-means-hourly.png', width = 8, height = 4, units = 'in')


# estimated vs. observed group differences (n - g)
group_means <- snakes %>%
  group_by(type, datetime, day, hour) %>%
  summarize(mean_temp = mean(temp)) %>%
  pivot_wider(id_cols = c(day, hour), names_from = type, values_from = mean_temp) %>%
  mutate(delta = N - G)

data_grid(snakes, type, hour) %>%
  add_predictions(fit1) %>%
  pivot_wider(id_cols = hour, values_from = pred, names_from = type) %>%
  mutate(delta = N - G) %>%
  ggplot(aes(x = hour, y = delta)) +
  geom_path() +
  # scale_y_continuous(limits = c(-9, 0.5)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_path(aes(group = day), data = group_means, alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-time-categorical-estimated-diffs-hourly.png', width = 8, height = 4, units = 'in')

# check residual autocorrelation
pacf_fn <- function(x){
  pacf_out <- pacf(x, plot = F)
  out <- bind_cols(pacf = pacf_out$acf[, 1, 1],
                   lag = pacf_out$lag[, , 1],
                   se = 2/sqrt(pacf_out$n.used))
  return(out)
}

# AR1 seems appropriate
fit1_df %>%
  nest(cols = resid, .by = c(id, type)) %>% 
  mutate(pacf = map(cols, pacf_fn)) %>%
  unnest(pacf) %>%
  ggplot(aes(x = lag)) +
  facet_wrap(~id) +
  geom_linerange(aes(ymin = 0, ymax = pacf)) +
  geom_ribbon(aes(ymin = -se, ymax = se), 
              fill = 'blue', 
              alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-time-categorical-resid-autocorrelation.png', width = 10, height = 10, units = 'in')

# check residual variance
fit1_df %>%
  group_by(id) %>%
  summarize(var.resid = var(resid)) 

## model 2: basis approximation for daily cycle
library(fda)

# compute fit
fit2 <- lm(temp ~ type*(fourier(hour, nbasis = 7, period = 23)[, -1]), data = snakes)

fit2_df <- snakes %>%
  add_predictions(fit2) %>%
  add_residuals(fit2)

anova(fit2)

# estimated means vs observations
fit2_df %>%
  ggplot(aes(x = datetime, y = temp, color = type, group = id)) +
  geom_path(alpha = 0.2) +
  geom_path(aes(y = pred))

ggsave(filename = 'img/exploratory/lm-time-smooth-fitted-means.png', width = 8, height = 4, units = 'in')


# same, by time of day
data_grid(snakes, type, hour = seq_range(hour, n = 100)) %>%
  add_predictions(fit2) %>%
  ggplot(aes(x = hour, y = pred, color = type)) +
  geom_path(aes(x = hour, y = pred)) +
  geom_path(aes(y = temp, group = interaction(id, day)), data = fit2_df, alpha = 0.05)

ggsave(filename = 'img/exploratory/lm-time-smooth-fitted-means-hourly.png', width = 8, height = 4, units = 'in')

# estimated vs. observed group differences (n - g)
data_grid(snakes, type, hour = seq_range(hour, n = 100)) %>%
  add_predictions(fit2) %>%
  pivot_wider(id_cols = hour, values_from = pred, names_from = type) %>%
  mutate(delta = N - G) %>%
  ggplot(aes(x = hour, y = delta)) +
  geom_path() +
  # scale_y_continuous(limits = c(-9, 0.5)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_path(aes(group = day), data = group_means, alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-time-smooth-estimated-diffs-hourly.png', width = 8, height = 4, units = 'in')

# AR1 still seems appropriate
fit2_df %>%
  nest(cols = resid, .by = c(id, type)) %>% 
  mutate(pacf = map(cols, pacf_fn)) %>%
  unnest(pacf) %>%
  ggplot(aes(x = lag)) +
  facet_wrap(~id) +
  geom_linerange(aes(ymin = 0, ymax = pacf)) +
  geom_ribbon(aes(ymin = -se, ymax = se), 
              fill = 'blue', 
              alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-time-smooth-resid-autocorrelation.png', width = 10, height = 10, units = 'in')


# check residual variance
fit2_df %>%
  group_by(id) %>%
  summarize(var.resid = var(resid)) 

## model 3: thinned/binned

# thin to day/night
night <- c(0:6, 19:23)
snakes_thinned <- snakes %>%
  mutate(time.of.day = factor(hour %in% night, 
                              labels = c('day', 'night')),
         day.lag = lag(day, n = 7)) %>%
  mutate(day.lag = replace_na(day.lag, 181)) %>%
  group_by(id, type, day.lag, time.of.day) %>%
  summarize(temp = median(temp))

# compute estimates
fit3 <- lm(temp ~ type*time.of.day, data = snakes_thinned)
fit3_df <- snakes_thinned %>%
  add_predictions(fit3) %>%
  add_residuals(fit3)

# estimated means vs observations
fit3_df %>%
  ggplot(aes(x = day.lag, 
             y = temp, 
             color = type, 
             group = id)) +
  facet_wrap(~time.of.day) +
  geom_path(alpha = 0.2) +
  geom_path(aes(y = pred))

ggsave(filename = 'img/exploratory/lm-time-binned-fitted-means.png', width = 8, height = 4, units = 'in')


# estimated group differences
ungroup(snakes_thinned) %>%
  data_grid(type, time.of.day) %>%
  add_predictions(fit3) %>%
  pivot_wider(id_cols = time.of.day, names_from = type, values_from = pred)

snakes_thinned %>%
  group_by(type, day.lag, time.of.day) %>%
  summarize(group_mean = mean(temp)) %>%
  ggplot(aes(x = time.of.day, y = group_mean, color = type)) +
  geom_boxplot()

ggsave(filename = 'img/exploratory/lm-time-binned-fitted-means-hourly.png', width = 5, height = 5, units = 'in')


# autocorrelation is weaker, but also series are much shorter
fit3_df %>%
  ungroup() %>%
  nest(cols = resid, .by = c(id, type, time.of.day)) %>% 
  mutate(pacf = map(cols, pacf_fn)) %>%
  unnest(pacf) %>%
  ggplot(aes(x = lag)) +
  facet_wrap(~id) +
  geom_linerange(aes(ymin = 0, ymax = pacf)) +
  geom_ribbon(aes(ymin = -se, ymax = se), 
              fill = 'blue', 
              alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-time-binned-resid-autocorrelation.png', width = 8, height = 4, units = 'in')

# residual variance 
fit3_df %>%
  group_by(id) %>%
  summarize(var.resid = var(resid)) 

## USING LONGER TIME RANGE
load('data/snakes-months.RData')

snakes %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = type)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '')

ggsave(filename = 'img/exploratory/raw-tb-60d.png', width = 8, height = 8, units = 'in')

snakes %>%
  ggplot(aes(x = datetime, 
             y = temp,
             color = type,
             group = id)) +
  geom_path(alpha = 0.2)

ggsave(filename = 'img/exploratory/raw-tb-60d-overlaid.png', width = 12, height = 6, units = 'in')

# compute fit
fit <- lm(temp ~ type*(fourier(hour, nbasis = 7, period = 23)[, -1])*(fourier(day, nbasis = 3, period = 365)[, -1]), data = snakes)

fit_df <- snakes %>%
  add_predictions(fit) %>%
  add_residuals(fit)

anova(fit)

# estimated means vs observations
fit_df %>%
  ggplot(aes(x = datetime, y = temp, color = type, group = id)) +
  geom_path(alpha = 0.2) +
  geom_path(aes(y = pred))

ggsave(filename = 'img/exploratory/lm-60d-fitted-means.png', width = 10, height = 4, units = 'in')

# monthly means
data_grid(snakes, type, day, hour = seq(0, 23, by = 3)) %>%
  add_predictions(fit) %>%
  ggplot(aes(x = day, y = pred, color = type)) +
  facet_wrap(~hour, nrow = 1) +
  geom_path()

ggsave(filename = 'img/exploratory/lm-60d-daily-means-by-hour.png', width = 12, height = 4, units = 'in')

# check residual autocorrelation
pacf_fn <- function(x){
  pacf_out <- pacf(x, plot = F)
  out <- bind_cols(pacf = pacf_out$acf[, 1, 1],
                   lag = pacf_out$lag[, , 1],
                   se = 2/sqrt(pacf_out$n.used))
  return(out)
}

# AR1 seems appropriate
fit_df %>%
  nest(cols = resid, .by = c(id, type)) %>% 
  mutate(pacf = map(cols, pacf_fn)) %>%
  unnest(pacf) %>%
  ggplot(aes(x = lag)) +
  facet_wrap(~id) +
  geom_linerange(aes(ymin = 0, ymax = pacf)) +
  geom_ribbon(aes(ymin = -se, ymax = se), 
              fill = 'blue', 
              alpha = 0.1)

ggsave(filename = 'img/exploratory/lm-60d-resid-autocorrelation.png', width = 10, height = 10, units = 'in')
