options(contrasts = rep('contr.sum', 2))
library(tidyverse)
library(modelr)
library(lsmeans)
library(nlme)
library(fda)
load('data/snakes.RData')

# add basis expansion
snakes_aug <- snakes %>%
  bind_cols(fb = fourier(snakes$hour, nbasis = 7, period = 23)[, -1]) %>%
  mutate(id = factor(id))

# define grouping structure for random effects
snakes_gr <- groupedData(temp ~ type | id, data = snakes_aug)

# define random effects structure
pdDg <- pdDiag(diag(6), ~fb - 1, data = snakes_aug)
reSt <- reStruct(list(id = pdDg))
corSt <- corAR1()

# fit model
fit <- lme(fixed = temp ~ type*fb,
           random = reSt,
           correlation = corSt,
           data = snakes_gr)

# fit summary
fit_sum <- summary(fit)

# fixed coefficient estimates
fit_sum$tTable[, 1:2]

# random effect variance estimates
VarCorr(fit)

# autoregressive correlation
fit$modelStruct$corStruct

# estimated random effects
ranef(fit)

# individual level fitted values (add ci?)
snakes %>%
  mutate(pred = predict(fit, level = 1)) %>%
  ggplot(aes(x = datetime, y = pred, group = id, color = type)) +
  geom_path(alpha = 0.6, color = 'black') +
  geom_path(aes(y = temp), alpha = 0.6) +
  facet_wrap(~id)

# same, on hourly time axis
snakes %>%
  mutate(pred = predict(fit, level = 1)) %>%
  ggplot(aes(x = hour, y = pred, group = interaction(id, day), color = type)) +
  geom_path(alpha = 0.6, color = 'black') +
  geom_path(aes(y = temp), alpha = 0.2) +
  facet_wrap(~id)

# population-level estimates w. ci 

grid_df <- data_grid(snakes, hour = seq_range(hour, 100), type) %>%
  mutate(fb = fourier(hour, nbasis = 7, period = 23)[, -1])
preds <- predict(fit, level = 0, newdata = grid_df)

grid_mx <- model.matrix(~type*fb, data = grid_df) 
pred_se <- diag(grid_mx %*% fit$varFix %*% t(grid_mx)) %>% sqrt()

pred_df <- grid_df %>% mutate(pred = preds, pred.se = pred_se)

ggplot(pred_df, aes(x = hour, y = pred, color = type)) +
  geom_path() +
  geom_ribbon(aes(ymin = pred - 2*pred.se, 
                  ymax = pred + 2*pred.se, 
                  fill = type),
              alpha = 0.3, color = NA) 

# F tests -- better to test main + interaction simultaneously?
anova(fit, type = 'sequential')
