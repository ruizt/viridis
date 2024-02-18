library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
library(emmeans)

## BODY TEMPERATURE (Tb)
#########################
load('data/fit-tb.RData')

# estimates
fit_sum <- summary(fit) # model summary
fit_sum$tTable[, 1:2] # fixed effects
VarCorr(fit) # random effect variances
coef(fit$modelStruct$corStruct, unconstrained = F) # autoregressive parameter
ranef(fit) # estimated random effects

# conditional inference
anova(fit, type = 'sequential')[-1,] 

# marginal means
fit_emm <- emmeans(fit, specs = 'type')
summary(fit_emm)

# difference in means
pairs(fit_emm) |> confint()

# raw data figure
p_raw <- snakes %>%
  mutate(id = paste(type, id, sep = '')) %>%
  ggplot(aes(x = hour, 
             y = temp, 
             color = day,
             group = interaction(type, id, day))) + 
  geom_path(alpha = 0.2) +
  facet_wrap(~id, nrow = 4) +
  scale_y_continuous(n.breaks = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'time', y = expr(t[b]))
p_raw

ggsave(p_raw, 
       filename = 'img/figs/tb-raw.png', 
       width = 8, height = 6, units = 'in', dpi = 400)

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
  geom_path(aes(y = temp), 
            data = snakes, 
            alpha = 0.03, 
            color = 'grey', 
            linewidth = 0.2) +
  geom_path(alpha = 0.5) +
  facet_wrap(~id.label, nrow = 4) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'time', y = expr(t[b]))
p_lev1_pred

ggsave(p_lev1_pred, 
       filename = 'img/figs/tb-level1-preds.png', 
       width = 8, height = 6, units = 'in', dpi = 400)

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

# estimated group means by hour*day
p_lev0_pred_hour <- level0_pred_df %>% 
  arrange(type, day, hour) %>%
  mutate(type = factor(type, labels = c('Gravid', 'Nongravid'))) %>%
  ggplot(aes(x = hour, y = pred, group = interaction(type, day))) +
  geom_ribbon(aes(ymin = pred - crit.val*se,
                  ymax = pred + crit.val*se,
                  fill = day), 
              # fill = 'grey',
              alpha = 0.01) +
  geom_path(aes(color = day), alpha = 0.5) +
  facet_wrap(~type, nrow = 1) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                         linewidth = 0.1)) +
  labs(x = 'time', y = expr(t[b]))
p_lev0_pred_hour

ggsave(p_lev0_pred_hour,
       filename = 'img/figs/tb-level0-preds.png', 
       width = 6, height = 3, units = 'in', dpi = 400)

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
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  geom_hline(yintercept=0) +
  labs(x = 'time', y = expr(t[b](Gravid) - t[b](Nongravid)))
p_diff

ggsave(p_diff,
       filename = 'img/figs/tb-level0-diffs.png', 
       width = 4, height = 3, units = 'in', dpi = 400)

rm(list = ls())

## ENVIRONMENTAL TEMPERATURE (Te)
###################################
load('data/fit-te.RData')

# estimates
fit_sum <- summary(fit) # model summary
fit_sum$tTable[, 1:2] # fixed effects
VarCorr(fit) # random effect variances
coef(fit$modelStruct$corStruct, unconstrained = F) # autoregressive parameter
ranef(fit) # estimated random effects

# marginal means and pairwise comparisons
fit_emm <- emmeans(fit, specs = c('location', 'treatment'))
summary(fit_emm) |> arrange(emmean)
pairs(fit_emm) |> confint()
emmeans(fit, specs = 'location') |> pairs() |> confint()
emmeans(fit, specs = 'treatment') |> pairs() |> confint()

# conditional inference
anova(fit, type = 'sequential')[-1,] 

# raw data plot
p_raw <- otm %>%
  rename(exposure = treatment) %>%
  ggplot(aes(x = hour, 
             y = temp, 
             color = exposure, 
             group = interaction(day, id))) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.2, linewidth = 0.2) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  labs(x = 'time', y = expr(t[e])) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1)
p_raw

ggsave(p_raw, filename = 'img/figs/otm-raw.png',
       width = 6, height = 3, units = 'in', dpi = 400)

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
  geom_path(alpha = 0.4, linewidth = 0.2) +
  facet_wrap(~location*site, nrow = 2) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 80, by = 20)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1) +
  labs(x = 'time', y = expr(hat(t)[e]))
p_lev1_pred

ggsave(p_lev1_pred, 
       filename = 'img/figs/te-level1-preds.png', 
       width = 6, height = 3, units = 'in', dpi = 400)

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

# predictions by treatment (comparing location)
p_lev0_pred_trt <- level0_preds %>%
  ggplot(aes(x = hour,
             y = pred, 
             group = interaction(day, location, treatment))) +
  geom_path(aes(color = location), alpha = 0.4, linewidth = 0.2) +
  geom_ribbon(aes(ymin = pred - crit.val*se, 
                  ymax = pred + crit.val*se,
                  fill = location),
              alpha = 0.005) +
  facet_wrap(~treatment) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.4))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1) +
  labs(x = 'time', y = expr(hat(t)[e]))
p_lev0_pred

ggsave(plot = p_lev0_pred_trt,
       filename = 'img/figs/te-level0-preds.png',
       width = 5, height = 2, units = 'in', dpi = 400)

# bonferroni correction for differences
crit.val <- qnorm(1 - (0.05/4032)/2)

# location differences by treatment
p_lev0_diffs <- level0_preds %>%
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
  labs(x = 'time', 
       y = expr(hat(t)[e](rookery) - hat(t)[e](prairie))) +
  theme_bw() +
  scale_y_continuous(n.breaks = 5) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  geom_hline(yintercept = 0, linewidth = 0.2)
p_lev0_diffs

ggsave(plot = p_lev0_diffs,
       filename = 'img/figs/te-level0-diffs.png',
       width = 5, height = 2, units = 'in', dpi = 400)


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
