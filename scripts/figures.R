library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
library(emmeans)
library(gridExtra)
library(patchwork)
source('scripts/model-fitting.R')
path <- 'results/img/'
res <- 500
ext <- '.tiff'

tpref <- gradient |> 
  ungroup() |> 
  group_by(repro) |>
  summarize(across(starts_with('tb'), mean)) |>
  mutate(type = factor(repro, 
                       levels = c('Pregnant', 'Non-Pregnant'),
                       labels = c('G', 'N')),
         location = factor(repro, 
                           levels = c('Pregnant', 'Non-Pregnant'),
                           labels = c('Rookery', 'Prairie'))) |>
  dplyr::select(-repro)

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
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', T[b], ' (\u00B0C)'))) +
  guides(color = guide_colorbar(title = 'Date'))

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
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(T)[b], ' (\u00B0C)', sep = ' '))) +
  guides(color = guide_colorbar(title = 'Date'))

s01 <- tb_raw + tb_fitted + plot_layout(nrow = 2)
ggsave(s01, filename = paste(path, 's01-tb', ext, sep = ''), 
       width = 8, height = 5, units = 'in', dpi = res)

## SUPPLEMENTAL FIGURE S02 ---------------------------------------------
te_raw <- te |>
  rename(exposure = treatment) |>
  ggplot(aes(x = hour, 
             y = temp, 
             color = exposure, 
             group = interaction(day, id))) +
  facet_wrap(~location*site, nrow = 2) +
  geom_path(alpha = 0.2, linewidth = 0.1) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', T[e], ' (\u00B0C)'))) +
  guides(color = guide_legend(title = '', override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2)

# data grid for individual level predictions
level1_grid_te <- data_grid(te,
                            id,
                            hour = seq_range(hour, 100), 
                            day = seq_range(day, 40)) |>
  left_join(distinct(dplyr::select(te, id, location, site, treatment)), by = 'id') |>
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# prediction grid
level1_preds_te <- level1_grid_te |> 
  bind_cols(pred = predict(fit_te, level = 1, newdata = level1_grid_te))

# fitted values by hour*day*location*site
te_fitted <- level1_preds_te |>
  rename(exposure = treatment) |>
  # mutate(exposure = fct_relevel(factor(exposure), c('Shaded', 'Exposed'))) |>
  ggplot(aes(x = hour, 
             y = pred,
             group = interaction(id, day, exposure), 
             color = exposure)) +
  geom_path(alpha = 0.4, linewidth = 0.1) +
  facet_wrap(~location*site, nrow = 2) +
  scale_y_continuous(breaks = seq(0, 80, by = 20)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(color = guide_legend(title = '', override.aes = list(alpha = 1))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(T)[e], ' (\u00B0C)')))

s02 <- te_raw + te_fitted + plot_layout(nrow = 2)
ggsave(s02, filename = paste(path, 's02-te', ext, sep = ''), 
       width = 6, height = 6, units = 'in', dpi = res)


## FIGURE 1 -----------------------------------------------------------

# data grid for group level predictions
level0_grid_df_tb <- data_grid(tb,
                               hour = seq_range(hour, 100), 
                               type,
                               day) |>
  mutate(fb.hour = fourier(hour, nbasis = 7, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

grid_mx_tb <- model.matrix(~type*fb.hour*fb.day, data = level0_grid_df_tb) 
pred_se_tb <- diag(grid_mx_tb %*% fit_tb$varFix %*% t(grid_mx_tb)) |> sqrt()

level0_pred_df_tb <- level0_grid_df_tb |> 
  bind_cols(pred = predict(fit_tb, level = 0, newdata = level0_grid_df_tb), 
            se = pred_se_tb) |>
  mutate(minute = floor((hour %% 1)*60),
         hour.int = floor(hour)) |>
  mutate(datetime = make_datetime(year = 2020, 
                                  day = day, 
                                  hour = hour.int, 
                                  min = minute)) 

# bonferroni correction for confidence bands
crit.val.tb <- qnorm(1 - (0.05/1344)/2)

# estimated group means by hour*day
tb_pred <- level0_pred_df_tb |> 
  left_join(tpref, by = 'type') |>
  arrange(type, day, hour) |>
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant')),
         date = as.Date(day, origin = '2019-12-31')) |>
  ggplot(aes(x = hour, y = pred, 
             group = interaction(type, day))) +
  # geom_ribbon(aes(ymin = pred - crit.val.tb*se,
  #                 ymax = pred + crit.val.tb*se,
  #                 fill = date), 
  #             # fill = 'grey',
  #             alpha = 0.01) +
  geom_ribbon(aes(x = hour, ymin = tb.q1, ymax = tb.q3), 
              fill = 'grey', alpha = 0.4, inherit.aes = F) +
  geom_path(aes(color = date), alpha = 0.5) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(T)[b], ' (\u00B0C)'))) +
  ylim(c(10, 35)) +
  guides(color = guide_colorbar(title = 'Date'),
         fill = guide_colorbar(title = 'Date'))

tb_avg <- tb |>
  left_join(tpref, by = 'type') |>
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |> 
  group_by(type, date = date(datetime), day, hour) |>
  summarize(temp = mean(temp),
            tb.q1 = unique(tb.q1),
            tb.q3 = unique(tb.q3)) |>
  ggplot(aes(x = hour, y = temp, 
             group = interaction(type, day))) +
  geom_ribbon(aes(x = hour, ymin = tb.q1, ymax = tb.q3), 
              fill = 'grey', alpha = 0.4, inherit.aes = F) +
  geom_path(alpha = 0.4, linewidth = 0.1) +
  geom_smooth(aes(x = hour, y = temp),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  scale_y_continuous(n.breaks = 6) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', T[b], ' (\u00B0C)'))) +
  ylim(c(10, 35))

te_avg <- te |>
  left_join(tpref, by = 'location') |>
  group_by(location, treatment, hour, date = date(datetime)) |>
  arrange(date, hour) |>
  summarize(temp = mean(temp),
            tb.q1 = unique(tb.q1),
            tb.q3 = unique(tb.q3)) |>
  ggplot(aes(x = hour, y = temp, 
             group = interaction(date, location, treatment))) +
  geom_ribbon(aes(x = hour, ymin = tb.q1, ymax = tb.q3), 
              fill = 'grey', alpha = 0.4, inherit.aes = F) +
  geom_path(aes(color = treatment), alpha = 0.3, linewidth = 0.1) +
  geom_smooth(aes(x = hour, y = temp, color = treatment),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  facet_wrap(~location, nrow = 1) +
  scale_y_continuous(n.breaks = 6, limits = c(-10, 70)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', T[e], ' (\u00B0C)'))) +
  guides(color = guide_none())

# data grid for level 0 predictions
level0_grid_te <- data_grid(te,
                         location,
                         treatment,
                         day = seq_range(day, 30),
                         hour = seq_range(hour, 100)) |>
  mutate(fb.hour = fourier(hour, nbasis = 5, period = 23)[, -1],
         fb.day = fourier(day, nbasis = 3, period = 365)[, -1])

# standard errors
grid_mx_te <- model.matrix(~treatment*location*fb.hour*fb.day, 
                        data = level0_grid_te) 
pred_se_te <- diag(grid_mx_te %*% fit_te$varFix %*% t(grid_mx_te)) |> sqrt()

# prediction grid
level0_preds_te <- level0_grid_te |>
  mutate(pred = predict(fit_te, level = 0, newdata = level0_grid_te),
         se = pred_se_te) |>
  dplyr::select(-starts_with('fb'))

# bonferroni correction for confidence bands
crit.val.te <- qnorm(1 - (0.05/8064)/2)

# predictions by location
te_pred <- level0_preds_te |>
  rename(exposure = treatment) |>
  left_join(tpref, by = 'location') |>
  # mutate(exposure = fct_relevel(factor(exposure), c('Shaded', 'Exposed'))) |>
  ggplot(aes(x = hour,
             y = pred, 
             group = interaction(day, location, exposure))) +
  geom_ribbon(aes(x = hour, ymin = tb.q1, ymax = tb.q3), 
              fill = 'grey', alpha = 0.4, inherit.aes = F) +
  geom_path(aes(color = exposure), alpha = 0.4, linewidth = 0.15) +
  # geom_ribbon(aes(ymin = pred - crit.val.te*se, 
  #                 ymax = pred + crit.val.te*se,
  #                 fill = exposure),
  #             alpha = 0.01) +
  facet_wrap(~location) +
  scale_y_continuous(n.breaks = 6, limits = c(-10, 70)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(T)[e], ' (\u00B0C)'))) +
  guides(fill = guide_none(),
         color = guide_legend(title = '', override.aes = list(alpha = 1,
                                                              linewidth = 1)))

f01 <- te_avg + te_pred + tb_avg + tb_pred + plot_layout(nrow = 2)

ggsave(f01, filename = paste(path, 'fig1-te-tb', ext, sep = ''), 
       width = 7.5, height = 4, units = 'in', dpi = res)

## FIGURE 2 -----------------------------------------------------------
  
db <- tb |>
  left_join(tpref, by = 'type') |>
  mutate(db.lo = temp - tb.q1,
         db.hi = temp - tb.q3) |>
  mutate(db = if_else(sign(db.lo)*sign(db.hi) == -1,
                      0,
                      if_else(sign(db.lo) == -1, 
                              db.lo, 
                              db.hi))) |>
  dplyr::select(id, type, datetime, day, hour, db)

db_avg <- db |>
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |> 
  group_by(type, date = date(datetime), day, hour) |>
  summarize(db = mean(db)) |>
  ggplot(aes(x = hour, y = db, 
             group = interaction(type, day))) +
  geom_path(alpha = 0.4, linewidth = 0.1) +
  geom_smooth(aes(x = hour, y = db),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  scale_y_continuous(n.breaks = 6, limits = c(-20, 2)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', d[b]))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.5)

level0_pred_df_db <- level0_pred_df_tb |>
  left_join(tpref, by = 'type') |>
  mutate(db.lo = pred - tb.q1,
         db.hi = pred - tb.q3) |>
  mutate(pred.db = if_else(sign(db.lo)*sign(db.hi) == -1,
                      0,
                      if_else(sign(db.lo) == -1, 
                              db.lo, 
                              db.hi))) |>
  dplyr::select(type, datetime, day, hour, pred.db, se)

db_pred <- level0_pred_df_db |> 
  arrange(type, day, hour) |>
  mutate(type = factor(type, labels = c('Pregnant', 'Nonpregnant')),
         date = as.Date(day, origin = '2019-12-31')) |>
  # mutate(type = fct_relevel(type, c('Pregnant', 'Nonpregnant'))) |>
  ggplot(aes(x = hour, y = pred.db, 
             group = interaction(type, day))) +
  geom_path(aes(color = date), alpha = 0.5) +
  facet_wrap(~fct_rev(type), nrow = 1) +
  scale_y_continuous(n.breaks = 6, limits = c(-20, 2)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(d)[b]))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.5) +
  guides(color = guide_colorbar(title = 'Date'))

de <- te |>
  left_join(tpref, by = 'location') |>
  mutate(de.lo = temp - tb.q1,
         de.hi = temp - tb.q3) |>
  mutate(de = if_else(sign(de.lo)*sign(de.hi) == -1,
                      0,
                      if_else(sign(de.lo) == -1, 
                              de.lo, 
                              de.hi))) |>
  dplyr::select(id, site, location, treatment, datetime, day, hour, de)

de_avg <- de |>
  group_by(location, treatment, hour, date = date(datetime)) |>
  arrange(date, hour) |>
  summarize(de = mean(de)) |>
  ggplot(aes(x = hour, y = de, 
             group = interaction(date, location, treatment))) +
  geom_path(aes(color = treatment), alpha = 0.3, linewidth = 0.1) +
  geom_smooth(aes(x = hour, y = de, color = treatment),
              method = 'loess', formula = 'y ~ x', 
              inherit.aes = F, span = 0.3, se = F, n = 200) +
  facet_wrap(~location, nrow = 1) +
  scale_y_continuous(n.breaks = 6, limits = c(-32, 40)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black', 
                                          linewidth = 0.2)) +
  labs(x = 'Hour', y = expr(paste('Observed ', d[e]))) +
  guides(color = guide_none()) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.5)

level0_preds_de <- level0_preds_te |>
  left_join(tpref, by = 'location') |>
  mutate(de.lo = pred - tb.q1,
         de.hi = pred - tb.q3) |>
  mutate(pred.de = if_else(sign(de.lo)*sign(de.hi) == -1,
                      0,
                      if_else(sign(de.lo) == -1, 
                              de.lo, 
                              de.hi))) |>
  dplyr::select(location, treatment, day, hour, pred.de)

de_pred <- level0_preds_de |>
  rename(exposure = treatment) |>
  # mutate(exposure = fct_relevel(factor(exposure), c('Shaded', 'Exposed'))) |>
  ggplot(aes(x = hour,
             y = pred.de, 
             group = interaction(day, location, exposure))) +
  geom_path(aes(color = exposure), alpha = 0.6, linewidth = 0.1) +
  facet_wrap(~location) +
  scale_y_continuous(n.breaks = 6, limits = c(-32, 40)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(color = guide_legend(title= '', 
                              override.aes = list(alpha = 1, 
                                                  linewidth = 0.5))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.5) +
  labs(x = 'Hour', y = expr(paste('Estimated ', hat(d)[e]))) 

f02 <- de_avg + de_pred + db_avg + db_pred + plot_layout(nrow = 2)

ggsave(f02, filename = paste(path, 'fig2-de-db', ext, sep = ''), 
       width = 7.5, height = 4, units = 'in', dpi = res)


## FIGURE 3 -----------------------------------------------------------

cut(0:23, breaks = c(0, 5, 9, 17, 21, 24), right = F) |> 
  fct_other(keep = c('[0,5)', '[9,17)', '[21,24)'), other_level = 'transition') |>
  fct_other(keep = c('transition', '[9,17)'), other_level = 'night') |>
  fct_recode(day = '[9,17)')

daytime_fn <- function(x){
  out <- cut(x, breaks = c(0, 9, 17, 24), right = F) |> 
    fct_other(keep = '[9,17)', other_level = 'night') |>
    fct_recode(day = '[9,17)')
  return(out)
}

daytime_fn_alt <- function(x){
  out <- cut(x, breaks = c(0, 5, 9, 17, 21, 24), right = F) |> 
    fct_other(keep = c('[0,5)', '[9,17)', '[21,24)'), other_level = 'transition') |>
    fct_other(keep = c('transition', '[9,17)'), other_level = 'Night') |>
    fct_recode(Day = '[9,17)')
  return(out)
}

tb_timeofday <- tb |> 
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag),
         type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |>
  group_by(type, date = date.adj, daytime) |>
  summarize(tb = mean(temp)) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = date, y = tb, linetype = type)) +
  facet_wrap(~daytime) +
  geom_path() +
  labs(x = '', y = expression(paste('Average ', T[b], ' (\u00B0C)', sep = ''))) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(linetype = guide_none()) +
  scale_y_continuous(limits = c(15, 33), n.breaks = 4)

tb_timeofday_avg <- tb |> 
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag),
         type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |>
  group_by(type, date = date.adj, daytime) |>
  summarize(tb = mean(temp)) |>
  group_by(type, daytime) |>
  summarize(tb.avg = mean(tb),
            tb.se = sd(tb)/sqrt(n())) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = daytime, y = tb.avg, shape = type)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = tb.avg - 8*tb.se, 
                     ymax = tb.avg + 8*tb.se,
                     linetype = type),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(linetype = guide_legend(title = 'Group'),
         shape = guide_legend(title = 'Group')) +
  labs(x = '', y = expression(paste('Average ', T[b], ' (\u00B0C)', sep = ''))) +
  scale_y_continuous(limits = c(15, 33), n.breaks = 4)

tb_dates <- tb |> pull(datetime) |> date() |> range()

te_timeofday <- te |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(te = mean(temp)) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = date, y = te, linetype = location, color = treatment)) +
  facet_wrap(~daytime) +
  geom_path() +
  labs(x = '', y = expression(paste('Average ', T[e], ' (\u00B0C)', sep = ''))) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(linetype = guide_none(),
         color = guide_none()) +
  scale_y_continuous(limits = c(0, 55), n.breaks = 6)

te_timeofday_avg <- te |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(te = mean(temp)) |>
  group_by(location, treatment, daytime) |>
  summarize(te.avg = mean(te),
            te.se = sd(te)/sqrt(n())) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = daytime, y = te.avg, 
             shape = location, 
             linetype = location, 
             color = treatment)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = te.avg - 8*te.se, 
                     ymax = te.avg + 8*te.se,
                     linetype = location),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  # facet_wrap(~treatment) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(color = guide_legend(title = 'Exposure'),
         shape = guide_legend(title = 'Location'),
         linetype = guide_legend(title = 'Location')) +
  labs(x = '', y = expression(paste('Average ', T[e], ' (\u00B0C)', sep = ''))) +
  scale_y_continuous(limits = c(0, 55), n.breaks = 6)

f03 <- te_timeofday + te_timeofday_avg + 
  tb_timeofday + tb_timeofday_avg + 
  plot_layout(nrow = 2, widths = c(3, 1))

ggsave(f03, filename = paste(path, 'fig3-timeofday-te-tb', ext, sep = ''), 
       width = 8, height = 5, units = 'in', dpi = res)

## FIGURE 4 -----------------------------------------------------------

db_timeofday <- db |> 
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag),
         type = fct_relevel(factor(type, labels = c('Pregnant', 'Nonpregnant')), 
                            "Nonpregnant", "Pregnant")) |>
  group_by(type, date = date.adj, daytime) |>
  summarize(db = mean(db)) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = date, y = db, linetype = type)) +
  facet_wrap(~daytime) +
  geom_path() +
  labs(x = '', y = expression(paste('Average ', d[b], sep = ''))) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(linetype = guide_none()) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2) +
  scale_y_continuous(limits = c(-16, 1), n.breaks = 4)

db_timeofday_avg <- db |> 
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag),
         type = fct_relevel(factor(type, labels = c('Pregnant', 'Nonpregnant')),
                            "Nonpregnant", "Pregnant")) |>
  group_by(type, date = date.adj, daytime) |>
  summarize(db = mean(db)) |>
  group_by(type, daytime) |>
  summarize(db.avg = mean(db),
            db.se = sd(db)/sqrt(n())) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = daytime, y = db.avg, shape = type)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = db.avg - 8*db.se, 
                     ymax = db.avg + 8*db.se,
                     linetype = type),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(linetype = guide_legend(title = 'Group'),
         shape = guide_legend(title = 'Group')) +
  labs(x = '', y = expression(paste('Average ', d[b], sep = ''))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2)  +
  scale_y_continuous(limits = c(-16, 1), n.breaks = 4)

de_timeofday <- de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = date, y = de, linetype = location, color = treatment)) +
  facet_wrap(~daytime) +
  geom_path() +
  labs(x = '', y = expression(paste('Average ', d[e], sep = ''))) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2)) +
  guides(linetype = guide_none(),
         color = guide_none()) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2)  +
  scale_y_continuous(limits = c(-30, 25), n.breaks = 6)

de_timeofday_avg <- de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  group_by(location, treatment, daytime) |>
  summarize(de.avg = mean(de),
            de.se = sd(de)/sqrt(n())) |>
  filter(daytime != 'transition') |>
  ggplot(aes(x = daytime, y = de.avg, 
             shape = location, 
             linetype = location, 
             color = treatment)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = de.avg - 5*de.se, 
                     ymax = de.avg + 5*de.se,
                     linetype = location),
                 width = 0.2,
                 position = position_dodge(width = 0.3)) +
  # facet_wrap(~treatment) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'black',
                                          linewidth = 0.2),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(color = guide_legend(title = 'Exposure'),
         shape = guide_legend(title = 'Location'),
         linetype = guide_legend(title = 'Location')) +
  labs(x = '', y = expression(paste('Average ', d[e], sep = ''))) +
  geom_hline(yintercept = 0, color = 'black', linewidth = 0.2)  +
  scale_y_continuous(limits = c(-30, 25), n.breaks = 6)

f04 <- de_timeofday + de_timeofday_avg +
  db_timeofday + db_timeofday_avg +
  plot_layout(nrow = 2, widths = c(3, 1))

ggsave(f04, filename = paste(path, 'fig4-timeofday-de-db', ext, sep = ''), 
       width = 8, height = 5, units = 'in', dpi = res)

## ADDITIONAL SUMMARIES --------------------------------------------------------

de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(location, treatment) |>
  summarize(de = mean(de)) |>
  write_csv('results/tbl/de-tbl.csv')

de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  group_by(location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  filter(daytime != 'transition') |>
  pivot_wider(names_from = location, values_from = de) |>
  mutate(diff = Prairie - Rookery) |>
  write_csv('results/tbl/de-tbl-daytime.csv')

de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  group_by(location, daytime) |>
  summarize(de = mean(de)) |>
  filter(daytime != 'transition') |>
  pivot_wider(names_from = location, values_from = de) |>
  mutate(diff = Prairie - Rookery) |>
  write_csv('results/tbl/de-tbl-pr.csv')

de |>
  filter(date(datetime) >= tb_dates[1],
         date(datetime) <= tb_dates[2]) |>
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag)) |>
  group_by(date = date.adj, location, treatment, daytime) |>
  summarize(de = mean(de)) |>
  group_by(treatment, daytime) |>
  summarize(de = mean(de)) |>
  filter(daytime != 'transition') |>
  pivot_wider(names_from = treatment, values_from = de) |>
  mutate(diff = Exposed - Shaded) |>
  write_csv('results/tbl/de-tbl-es.csv')

db |> 
  mutate(daytime = daytime_fn_alt(hour),
         datetime.lag = datetime - hms('9:00:00'),
         date.adj = date(datetime.lag),
         type = factor(type, labels = c('Pregnant', 'Nonpregnant'))) |>
  group_by(type, date = date.adj, daytime) |>
  summarize(db = mean(db)) |>
  group_by(type, daytime) |>
  summarize(db.avg = mean(db)) |>
  filter(daytime != 'transition') |>
  pivot_wider(names_from = type, values_from = db.avg) |>
  mutate(diff = Pregnant - Nonpregnant) |>
  write_csv('results/tbl/db-tbl.csv')
