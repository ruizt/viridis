library(tidyverse)
library(modelr)
library(nlme)
library(fda)
load('data/snakes-60d.RData')
grad <- read_csv('data/gradient-summary.csv')
options(contrasts = rep('contr.sum', 2))

# raw data figure
p_raw <- snakes %>%
  ggplot(aes(x = datetime, 
             y = temp, 
             color = type)) + 
  geom_path(alpha = 0.5) +
  facet_wrap(~id, nrow = 4) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = '', y = expr(t[b]))

ggsave(p_raw, 
       filename = 'img/results/tb-raw-60d.png', 
       width = 8, height = 8, units = 'in')

# add basis expansions
snakes_aug <- snakes %>%
  bind_cols(fb.hour = fourier(snakes$hour, nbasis = 7, period = 23)[, -1],
            fb.day = fourier(snakes$day, nbasis = 3, period = 365)[, -1]) %>%
  mutate(id = factor(id))

# define grouping structure by snake for random effects
snakes_gr <- groupedData(temp ~ type | id, data = snakes_aug)

# define random effects structure
pdDg <- pdDiag(diag(6), ~fb.hour - 1, data = snakes_aug)
reSt <- reStruct(list(id = pdDg))
corSt <- corAR1()

# fit model
fit <- lme(fixed = temp ~ type*fb.hour*fb.day,
           random = reSt,
           correlation = corSt,
           data = snakes_gr)

save(list = ls(), file = 'data/fit-tb.RData')

# estimates
fit_sum <- summary(fit) # model summary
fit_sum$tTable[, 1:2] # fixed effects
VarCorr(fit) # random effect variances
fit$modelStruct$corStruct # autoregressive parameter
ranef(fit) # estimated random effects

# conditional inference
anova(fit, type = 'sequential')[-1,] 


## model-based visualizations
##############################
load('data/fit-tb.RData')

# data grid for individual level predictions
level1_grid_df <- data_grid(snakes,
                            id = unique(id),
                            hour = seq_range(hour, 100), 
                            # type,
                            day) %>%
  left_join(distinct(dplyr::select(snakes, id, type)), by = 'id') %>%
  mutate(fb.hour = fourier(hour, nbasis = 7, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# prediction grid
level1_pred_df <- level1_grid_df %>% 
  bind_cols(pred = predict(fit, level = 1, newdata = level1_grid_df))

# fitted values by hour*day*snake
p_lev1_pred <- level1_pred_df %>%
  mutate(id.label = paste(type, id, sep = '')) %>%
  ggplot(aes(x = hour, y = pred, group = interaction(id, day), color = day)) +
  geom_path(aes(y = temp), data = snakes, alpha = 0.02, color = 'darkgrey') +
  geom_path(alpha = 0.5) +
  facet_wrap(~id.label, nrow = 4) +
  theme_bw() +
  labs(x = 'hour of day', y = expr(t[b]))

ggsave(p_lev1_pred, filename = 'img/results/tb-level1-preds.png', width = 8, height = 8, units = 'in')

# same, without raw data
p_lev1_pred_nodata <- level1_pred_df %>%
  mutate(id.label = paste(type, id, sep = '')) %>%
  ggplot(aes(x = hour, y = pred, group = interaction(id, day), color = day)) +
  # geom_path(aes(y = temp), data = snakes, alpha = 0.02, color = 'grey') +
  geom_path(alpha = 0.4) +
  facet_wrap(~id.label, nrow = 4) +
  theme_bw() +
  labs(x = 'hour of day', y = expr(t[b]))

ggsave(p_lev1_pred_nodata, 
       filename = 'img/results/tb-level1-preds-nodata.png', 
       width = 8, height = 8, units = 'in')

# data grid for group level predictions
level0_grid_df <- data_grid(snakes,
                            hour = seq_range(hour, 100), 
                            type,
                            day) %>%
  mutate(fb.hour = fourier(hour, nbasis = 7, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])
grid_mx <- model.matrix(~type*fb.hour*fb.day, data = level0_grid_df) 
pred_se <- diag(grid_mx %*% fit$varFix %*% t(grid_mx)) %>% sqrt()
level0_pred_df <- level0_grid_df %>% 
  bind_cols(pred = predict(fit, level = 0, newdata = level0_grid_df), 
            se = pred_se) %>%
  mutate(minute = floor((hour %% 1)*60),
         hour.int = floor(hour)) %>%
  mutate(datetime = make_datetime(year = 2020, 
                                  day = day, 
                                  hour = hour.int, 
                                  min = minute)) 

# bonferroni correction for confidence bands
crit.val <- qnorm(1 - (0.05/1344)/2)

# estimated group means by date
p_lev0_pred_date <- level0_pred_df %>% 
  dplyr::select(datetime, type, pred, se) %>%
  arrange(datetime) %>%
  ggplot(aes(x = datetime, y = pred, group = type)) +
  geom_path(aes(color = type)) +
  geom_ribbon(aes(ymin = pred - crit.val*se,
                  ymax = pred + crit.val*se,
                  fill = type), alpha = 0.2) +
  theme_bw() +
  labs(x = '', y = expr(t[b]))

ggsave(p_lev0_pred_date, 
       filename = 'img/results/tb-level0-preds-date.png', 
       width = 8, height = 4, units = 'in')

# estimated group means by hour*day
p_lev0_pred_hour <- level0_pred_df %>% 
  arrange(type, day, hour) %>%
  ggplot(aes(x = hour, y = pred, group = interaction(type, day))) +
  geom_ribbon(aes(ymin = pred - crit.val*se,
                  ymax = pred + crit.val*se), 
              fill = 'grey',
              alpha = 0.05) +
  geom_path(aes(color = day), alpha = 0.7) +
  theme_bw() +
  labs(x = 'hour of day', y = expr(t[b]))

ggsave(p_lev0_pred_hour,
       filename = 'img/results/tb-level0-preds-hourly.png', 
       width = 5, height = 4, units = 'in')

# estimated difference (G-NG)
p_diff <- level0_pred_df %>%
  dplyr::select(hour, day, datetime, pred, se, type) %>%
  pivot_wider(names_from = type, values_from = c(pred, se), names_sep = '.') %>%
  mutate(diff = pred.G - pred.N,
         diff.se = se.G + se.N) %>%
  ggplot(aes(x = hour, y = diff, group = day)) +
  geom_ribbon(aes(ymin = diff - crit.val*diff.se,
                  ymax = diff + crit.val*diff.se,
                  fill = day), 
              # fill = 'grey',
              alpha = 0.01) +
  geom_path(aes(color = day), alpha = 0.6) +
  theme_bw() +
  geom_hline(yintercept=0) +
  labs(x = 'hour of day', y = expr(t[b](G) - t[b](NG)))

ggsave(p_diff,
       filename = 'img/results/tb-level0-diffs-hourly.png', 
       width = 5, height = 4, units = 'in')

tpref <- grad %>% 
  group_by(Repro) %>% 
  dplyr::summarize(q1 = mean(q1), q3 = mean(q3), mid = mean(median)) %>%
  bind_cols(type = c('N', 'G')) %>%
  dplyr::select(-Repro)

p_pred_db_seasonal <- level0_pred_df %>%
  dplyr::select(hour, day, type, pred, se) %>%
  left_join(tpref, by = 'type') %>%
  mutate(pred.de = pred - mid) %>%
  ggplot(aes(x = hour, y = pred.de, group = interaction(day, type))) +
  geom_ribbon(aes(ymin = pred.de - crit.val*se,
                  ymax = pred.de + crit.val*se,
                  fill = type),
              alpha = 0.01) +
  geom_path(aes(color = type), alpha = 0.6) +
  geom_ribbon(aes(ymin = q1 - mid, ymax = q3 - mid, fill = type), alpha = 0.01) +
  theme_bw() +
  geom_hline(yintercept=0) +
  labs(x = 'hour of day', y = expr(d[b]))

ggsave(p_pred_db_seasonal,
       filename = 'img/results/db-level0-preds-hourly.png', 
       width = 5, height = 4, units = 'in')


save(list = ls(), file = 'data/tb-results.RData')
