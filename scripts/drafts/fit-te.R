library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
load('data/otm.RData')
grad <- read_csv('data/gradient-summary.csv')
options(contrasts = rep('contr.sum', 2))

# plot raw data by hour
otm %>%
  ggplot(aes(x = hour, y = temp, color = treatment, group = interaction(day, id))) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.1)

ggsave(filename = 'img/exploratory/otm-raw-hourly.png',
       width = 12, height = 5, units = 'in')

# basis expansions for time and day
otm_aug <- otm %>%
  bind_cols(fb.hour = fourier(otm$hour, nbasis = 5, period = 23)[, -1],
            fb.day = fourier(otm$day, nbasis = 3, period = 365)[,-1]) 

# visualize fit of fixed effects model
fit_lm <- lm(temp ~ location*treatment*fb.hour*fb.day, data = otm_aug)

otm_grid <- data_grid(otm, 
          location = unique(location), 
          treatment = unique(treatment),
          day = unique(day), 
          hour = seq_range(hour, n = 100))

otm_grid_aug <- otm_grid %>%
  bind_cols(fb.hour = fourier(otm_grid$hour, nbasis = 5, period = 23)[, -1],
            fb.day = fourier(otm_grid$day, nbasis = 3, period = 365)[,-1]) 

otm_grid_aug %>%
  add_predictions(fit_lm) %>%
  ggplot(aes(x = hour, 
             y = pred, 
             color = treatment, 
             group = interaction(day, treatment, location))) +
  facet_wrap(~location, nrow = 1) +
  geom_path(alpha = 0.2)

# check residual autocorrelation -- a few seem to be arma not ar
pacf_fn <- function(x){
  pacf_out <- pacf(x, plot = F)
  out <- bind_cols(pacf = pacf_out$acf[, 1, 1],
                   lag = pacf_out$lag[, , 1],
                   se = 2/sqrt(pacf_out$n.used))
  return(out)
}

otm_aug %>%
  add_residuals(fit_lm) %>%
  nest(cols = resid, .by = c(id, site)) %>% 
  mutate(pacf = map(cols, pacf_fn)) %>%
  unnest(pacf) %>%
  ggplot(aes(x = lag)) +
  facet_wrap(~id) +
  geom_linerange(aes(ymin = 0, ymax = pacf)) +
  geom_ribbon(aes(ymin = -se, ymax = se), 
              fill = 'blue', 
              alpha = 0.1)

# random effects structure
otm_gr <- groupedData(temp ~ treatment | site, data = otm_aug)
pdDg <- pdDiag(diag(10), ~ fb.hour*treatment - 1, data = otm_gr)
reSt <- reStruct(list(id = pdDg))
corSt <- corAR1(value = 0.8, form = ~ 1 | id)

# fit model
fit <- lme(fixed = temp ~ location*treatment*fb.hour*fb.day,
           random = reSt,
           correlation = corSt,
           data = otm_gr)

save(fit, file = 'data/fit-te.RData')

# estimates
fit_sum <- summary(fit) # model summary
fit_sum$tTable[, 1:2] # fixed effects
VarCorr(fit) # random effect variances
fit$modelStruct$corStruct # autoregressive parameter
ranef(fit) # estimated random effects
lsmeans::lsmeans(fit, 'location')
lsmeans::lsmeans(fit, 'treatment')

# conditional inference
anova(fit, type = 'sequential')[-1,] 

# data grid for individual level predictions
level1_grid <- data_grid(otm,
                         id,
                         hour = seq_range(hour, 100), 
                         day = seq_range(day, 40)) %>%
  left_join(distinct(dplyr::select(otm, id, location, site, treatment)), by = 'id') %>%
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# prediction grid
level1_preds <- level1_grid %>% 
  bind_cols(pred = predict(fit, level = 1, newdata = level1_grid))

# fitted values by hour*day*location*site
p_lev1_pred <- level1_preds %>%
  ggplot(aes(x = hour, 
             y = pred,
             group = interaction(id, day, treatment), 
             color = treatment)) +
  geom_path(alpha = 0.3) +
  facet_wrap(~location*site, nrow = 2) +
  theme_bw() +
  labs(x = 'hour of day', y = expr(t[e])) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey')) +
  geom_hline(yintercept = 0) +
  labs(x = 'time of day', y = expr(hat(t)[e]))
p_lev1_pred

ggsave(p_lev1_pred, filename = 'img/results/te-level1-preds.png', width = 12, height = 6, units = 'in')

# data grid for level 0 predictions
level0_grid <- data_grid(otm,
                         location,
                         treatment,
                         day = seq_range(day, 30),
                         hour = seq_range(hour, 100)) %>%
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# standard errors
grid_mx <- model.matrix(~treatment*location*fb.hour*fb.day, 
                        data = level0_grid) 
pred_se <- diag(grid_mx %*% fit$varFix %*% t(grid_mx)) %>% sqrt()

# prediction grid
level0_preds <- level0_grid %>%
  mutate(pred = predict(fit, level = 0, newdata = level0_grid),
         se = pred_se) %>%
  dplyr::select(-starts_with('fb'))

# bonferroni correction for confidence bands
crit.val <- qnorm(1 - (0.05/8064)/2)

# predictions by location (comparing treatment)
p_lev0_pred_loc <- level0_preds %>%
  ggplot(aes(x = hour,
             y = pred, 
             color = treatment,
             group = interaction(day, location, treatment))) +
  geom_path(alpha = 0.3) +
  geom_ribbon(aes(ymin = pred - crit.val*se, 
                  ymax = pred + crit.val*se,
                  color = NULL,
                  fill = treatment),
              alpha = 0.01) +
  facet_wrap(~location) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey')) +
  geom_hline(yintercept = 0) +
  labs(x = 'time of day', y = expr(hat(t)[e]))
p_lev0_pred_loc

ggsave(plot = p_lev0_pred_loc,
       filename = 'img/results/te-level0-preds-byloc.png',
       width = 7, height = 4, units = 'in')

# predictions by treatment (comparing location)
p_lev0_pred_trt <- level0_preds %>%
  ggplot(aes(x = hour,
             y = pred, 
             color = location,
             group = interaction(day, location, treatment))) +
  geom_path(alpha = 0.3) +
  geom_ribbon(aes(ymin = pred - crit.val*se, 
                  ymax = pred + crit.val*se,
                  color = NULL,
                  fill = location),
              alpha = 0.005) +
  facet_wrap(~treatment) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey')) +
  geom_hline(yintercept = 0) +
  labs(x = 'time of day', y = expr(hat(t)[e]))
p_lev0_pred_trt

ggsave(plot = p_lev0_pred_trt,
       filename = 'img/results/te-level0-preds-bytrt.png',
       width = 7, height = 4, units = 'in')

# bonferroni correction for differences
crit.val <- qnorm(1 - (0.05/4032)/2)

# location differences by treatment
p_lev0_locdiffs <- level0_preds %>%
  pivot_wider(values_from = c(pred, se), 
              names_from = location, 
              names_sep = '.') %>%
  mutate(pred.diff = pred.Rookery - pred.Prairie,
         pred.diff.se = sqrt(se.Prairie^2 + se.Rookery^2)) %>%
  ggplot(aes(x = hour, y = pred.diff, 
             group = interaction(treatment, day))) +
  geom_ribbon(aes(ymin = pred.diff - crit.val*pred.diff.se,
                  ymax = pred.diff + crit.val*pred.diff.se,
                  fill = day),
              alpha = 0.005) +
  geom_path(aes(color = day)) +
  facet_wrap(~treatment) +
  labs(x = 'time of day', y = expr(hat(t)[e](rookery) - hat(t)[e](prairie))) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey')) +
  geom_hline(yintercept = 0)
p_lev0_locdiffs

ggsave(plot = p_lev0_locdiffs,
       filename = 'img/results/te-level0-loc-diffs.png',
       width = 7, height = 4, units = 'in')


# treatment differences by location
p_lev0_trtdiffs <- level0_preds %>%
  pivot_wider(values_from = c(pred, se), 
              names_from = treatment, 
              names_sep = '.') %>%
  mutate(pred.diff = pred.Exposed - pred.Shaded,
         pred.diff.se = sqrt(se.Exposed^2 + se.Shaded^2)) %>%
  ggplot(aes(x = hour, y = pred.diff, 
             group = interaction(location, day))) +
  geom_ribbon(aes(ymin = pred.diff - crit.val*pred.diff.se,
                  ymax = pred.diff + crit.val*pred.diff.se,
                  fill = day),
              alpha = 0.005) +
  geom_path(aes(color = day), alpha = 0.5) +
  facet_wrap(~location) +
  labs(x = 'time of day', y = expr(hat(t)[e](exposed) - hat(t)[e](shaded))) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey')) +
  geom_hline(yintercept = 0)
p_lev0_trtdiffs

ggsave(plot = p_lev0_trtdiffs,
       filename = 'img/results/te-level0-trt-diffs.png',
       width = 7, height = 4, units = 'in')
