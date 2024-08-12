library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
options(contrasts = rep('contr.sum', 2))
load('viridis-data.RData')

## BODY TEMPERATURES (Tb)
##########################

# add basis expansions 
tb_aug <- tb %>%
  bind_cols(fb.hour = fourier(tb$hour, nbasis = 7, period = 23)[, -1],
            fb.day = fourier(tb$day, nbasis = 3, period = 365)[, -1]) %>%
  mutate(id = factor(id)) 

# define grouping structure by snake for random effects
tb_gr <- groupedData(temp ~ type | id, data = tb_aug)

# define random effects structure
pdDg_tb <- pdDiag(diag(6), ~fb.hour - 1, data = tb_aug)
reSt_tb <- reStruct(list(id = pdDg_tb))
corSt_tb <- corAR1(value = 0.8, form = ~ 1 | id)

# fit model
fit_tb <- lme(fixed = temp ~ type*fb.hour*fb.day,
           random = reSt_tb,
           correlation = corSt_tb,
           data = tb_gr)

## ENVIRONMENTAL TEMPERATURES (Te)
####################################

# add basis expansions for time and day
te_aug <- te %>%
  bind_cols(fb.hour = fourier(te$hour, nbasis = 5, period = 23)[, -1],
            fb.day = fourier(te$day, nbasis = 3, period = 365)[,-1]) 

# random effects structure
te_gr <- groupedData(temp ~ treatment | site, data = te_aug)
pdDg_te <- pdDiag(diag(8), ~ fb.hour:treatment - 1, data = te_gr)
reSt_te <- reStruct(list(id = pdDg_te))
corSt_te <- corAR1(value = 0.8, form = ~ 1 | id)

# fit model
fit_te <- lme(fixed = temp ~ location*treatment*fb.hour*fb.day,
           random = reSt_te,
           correlation = corSt_te,
           data = te_gr)