library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
library(emmeans)
library(gridExtra)
load('results/fit-tb.RData')
load('results/fit-te.RData')

## SUPPLEMENTAL FIGURE S01 -------------------------------------------------

# raw data plot
tb_raw <- tb |>
  distinct(id, type) |>
  group_by(type) |>
  mutate(newid = row_number()) |>
  right_join(tb) |>
  mutate(type = factor(type, levels = c('G', 'N'), 
                       labels = c('Pregnant', 'Nonpregnant'))) |>
  ggplot(aes(x = hour, 
             y = temp, 
             color = date(datetime),
             group = interaction(type, newid, day))) + 
  geom_path(alpha = 0.2) +
  facet_grid(rows = vars(type), cols = vars(newid)) +
  scale_y_continuous(n.breaks = 4) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(t[b])) +
  guides(color = guide_colorbar(title = 'date'))

# data grid for individual level predictions
tb_level1_grid_df <- data_grid(tb,
                               id = unique(id),
                               hour = seq_range(hour, 100), 
                               # type,
                               day) |>
  left_join(distinct(dplyr::select(tb, id, type)), by = 'id') |>
  mutate(fb.hour = fourier(hour, nbasis = 7, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# prediction grid
tb_level1_pred_df <- tb_level1_grid_df |> 
  bind_cols(pred = predict(fit_tb, level = 1, newdata = tb_level1_grid_df))

# fitted values by hour*day*snake
tb_fitted <- tb_level1_pred_df |>
  distinct(id, type) |>
  group_by(type) |>
  mutate(newid = row_number()) |>
  right_join(tb_level1_pred_df) |>
  mutate(type = factor(type, levels = c('G', 'N'), 
                       labels = c('Pregnant', 'Nonpregnant'))) |>
  ggplot(aes(x = hour, y = pred, 
             group = interaction(id, day), 
             color = as.Date(day, origin = '2019-12-31'))) +
  geom_path(alpha = 0.5) +
  facet_grid(row = vars(type), col = vars(newid)) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(hat(t)[b])) +
  guides(color = guide_colorbar(title = 'date'))

s01 <- gridExtra::grid.arrange(tb_raw, tb_fitted, nrow = 2)
ggsave(s01, filename = 'results/s01-tb.tiff', 
       width = 12, height = 8, units = 'in', dpi = 450)

## SUPPLEMENTAL FIGURE S02 ---------------------------------------------
te_raw <- te %>%
  rename(exposure = treatment) %>%
  ggplot(aes(x = hour, 
             y = temp, 
             color = exposure, 
             group = interaction(day, id))) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.2, linewidth = 0.2) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(t[e])) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1)

# data grid for individual level predictions
level1_grid_te <- data_grid(te,
                            id,
                            hour = seq_range(hour, 100), 
                            day = seq_range(day, 40)) %>%
  left_join(distinct(dplyr::select(te, id, location, site, treatment)), by = 'id') %>%
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# prediction grid
level1_preds_te <- level1_grid_te %>% 
  bind_cols(pred = predict(fit_te, level = 1, newdata = level1_grid_te))

# fitted values by hour*day*location*site
te_fitted <- level1_preds_te %>%
  rename(exposure = treatment) |>
  ggplot(aes(x = hour, 
             y = pred,
             group = interaction(id, day, exposure), 
             color = exposure)) +
  geom_path(alpha = 0.4, linewidth = 0.2) +
  facet_wrap(~location*site, nrow = 2) +
  scale_y_continuous(breaks = seq(0, 80, by = 20)) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1) +
  labs(x = 'hour', y = expr(hat(t)[e]))

s02 <- grid.arrange(te_raw, te_fitted, nrow = 2)
ggsave(s02, filename = 'results/s02-te.tiff', 
       width = 12, height = 10, units = 'in', dpi = 450)


## FIGURE 1 -------------------------------------------------

# data grid for group level predictions
level0_grid_df_tb <- data_grid(tb,
                               hour = seq_range(hour, 100), 
                               type,
                               day) %>%
  mutate(fb.hour = fourier(hour, nbasis = 7, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

grid_mx_tb <- model.matrix(~type*fb.hour*fb.day, data = level0_grid_df_tb) 
pred_se_tb <- diag(grid_mx_tb %*% fit_tb$varFix %*% t(grid_mx_tb)) %>% sqrt()

level0_pred_df_tb <- level0_grid_df_tb %>% 
  bind_cols(pred = predict(fit_tb, level = 0, newdata = level0_grid_df_tb), 
            se = pred_se_tb) %>%
  mutate(minute = floor((hour %% 1)*60),
         hour.int = floor(hour)) %>%
  mutate(datetime = make_datetime(year = 2020, 
                                  day = day, 
                                  hour = hour.int, 
                                  min = minute)) 

# bonferroni correction for confidence bands
crit.val.tb <- qnorm(1 - (0.05/1344)/2)

# estimated group means by hour*day
tb_pred <- level0_pred_df_tb %>% 
  arrange(type, day, hour) %>%
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant')),
         date = as.Date(day, origin = '2019-12-31')) %>%
  ggplot(aes(x = hour, y = pred, 
             group = interaction(type, day))) +
  geom_ribbon(aes(ymin = pred - crit.val.tb*se,
                  ymax = pred + crit.val.tb*se,
                  fill = date), 
              # fill = 'grey',
              alpha = 0.01) +
  geom_path(aes(color = date), alpha = 0.5) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(hat(t)[b])) +
  ylim(c(10, 35))

tb_avg <- tb |>
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |> 
  group_by(type, date = date(datetime), day, hour) |>
  summarize(temp = mean(temp)) |>
  ggplot(aes(x = hour, y = temp, 
             group = interaction(type, day))) +
  geom_path(alpha = 0.05) +
  geom_smooth(aes(x = hour, y = temp),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(t[b])) +
  ylim(c(10, 35))

te_avg <- te |>
  group_by(location, treatment, hour, date = date(datetime)) |>
  arrange(date, hour) |>
  summarize(temp = mean(temp)) |>
  ggplot(aes(x = hour, y = temp, 
             group = interaction(date, location, treatment))) +
  geom_path(aes(color = treatment), alpha = 0.05) +
  geom_smooth(aes(x = hour, y = temp, color = treatment),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  facet_wrap(~location, nrow = 1) +
  scale_y_continuous(n.breaks = 6, limits = c(-10, 70)) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey', 
                                          linewidth = 0.1)) +
  labs(x = 'hour', y = expr(t[e])) +
  guides(color = guide_none())

# data grid for level 0 predictions
level0_grid_te <- data_grid(te,
                         location,
                         treatment,
                         day = seq_range(day, 30),
                         hour = seq_range(hour, 100)) %>%
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# standard errors
grid_mx_te <- model.matrix(~treatment*location*fb.hour*fb.day, 
                        data = level0_grid_te) 
pred_se_te <- diag(grid_mx_te %*% fit_te$varFix %*% t(grid_mx_te)) %>% sqrt()

# prediction grid
level0_preds_te <- level0_grid_te %>%
  mutate(pred = predict(fit_te, level = 0, newdata = level0_grid_te),
         se = pred_se_te) %>%
  dplyr::select(-starts_with('fb'))

# bonferroni correction for confidence bands
crit.val.te <- qnorm(1 - (0.05/8064)/2)

# predictions by treatment (comparing location)
te_pred <- level0_preds_te %>%
  ggplot(aes(x = hour,
             y = pred, 
             group = interaction(day, location, treatment))) +
  geom_path(aes(color = treatment), alpha = 0.4, linewidth = 0.2) +
  geom_ribbon(aes(ymin = pred - crit.val.te*se, 
                  ymax = pred + crit.val.te*se,
                  fill = treatment),
              alpha = 0.01) +
  facet_wrap(~location) +
  scale_y_continuous(n.breaks = 6, limits = c(-10, 70)) +
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey',
                                          linewidth = 0.1)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.4))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.1) +
  labs(x = 'hour', y = expr(hat(t)[e])) 

f01 <- grid.arrange(tb_avg, tb_pred, te_avg, te_pred, nrow = 2)
ggsave(f01, filename = 'results/fig1-te-tb.tiff', 
       width = 12, height = 6, units = 'in', dpi = 450)

