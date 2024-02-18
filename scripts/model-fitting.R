library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
options(contrasts = rep('contr.sum', 2))

## BODY TEMPERATURES (Tb)
##########################

# load data
load('data/snakes-60d.RData')

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
corSt <- corAR1(value = 0.8, form = ~ 1 | id)

# fit model
fit <- lme(fixed = temp ~ type*fb.hour*fb.day,
           random = reSt,
           correlation = corSt,
           data = snakes_gr)

# save environment
save(list = ls(), file = 'data/fit-tb.RData')

# clear environment
rm(list = ls())

## ENVIRONMENTAL TEMPERATURES (Te)
####################################

# load data
load('data/otm.RData')

# add basis expansions for time and day
otm_aug <- otm %>%
  bind_cols(fb.hour = fourier(otm$hour, nbasis = 5, period = 23)[, -1],
            fb.day = fourier(otm$day, nbasis = 3, period = 365)[,-1]) 

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

# save fitted model
save(list = ls(), file = 'data/fit-te.RData')

# clear environment
rm(list = ls())
