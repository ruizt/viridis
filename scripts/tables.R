## ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(modelr)
library(fda)
library(nlme)
library(emmeans)
library(pander)
load('results/fit.RData')

## ------------------------------------------------------------
fit_emm_tb <- emmeans(fit_tb, specs = 'type')

group_means_tb <- summary(fit_emm_tb) |>
  tibble() |>
  rename(Group = type, 
         Estimate = emmean) |>
  mutate(CI = paste('(', round(lower.CL, 2), 
                          ', ', round(upper.CL, 2), ')')) |>
  dplyr::select(Group, Estimate, SE, CI)

contrasts_tb <- pairs(fit_emm_tb) |> 
  confint() |> 
  tibble() |>
  rename(Group = contrast, 
         Estimate = estimate) |>
  mutate(CI = paste('(', round(lower.CL, 2), 
                          ', ', round(upper.CL, 2), ')')) |>
  dplyr::select(Group, Estimate, SE, CI) 

bind_rows(group_means_tb, contrasts_tb) |>
  write_csv(file = 'results/tbl/tb-means.csv')

## ------------------------------------------------------------
anova_tb <- anova(fit_tb, type = 'sequential')[-1,] 
rownames(anova_tb) <- c('Group', 'Hour', 'Day', 'Group:Hour', 'Group:Day', 'Hour:Day', 'Group:Hour:Day')
anova_tb |>
  rownames_to_column(var = 'Term') |>
  write_csv(file = 'results/tbl/tb-anova.csv')

## ------------------------------------------------------------
emmeans(fit_te, ~ location*treatment) |>
  as_tibble() |>
  rename(Location = location,
         Exposure = treatment,
         Estimate = emmean) |>
  mutate(CI = paste('(', round(lower.CL, 2), 
                          ', ', round(upper.CL, 2), ')')) |>
  dplyr::select(Location, Exposure, Estimate, SE, CI) |>
  write_csv(file = 'results/tbl/te-means.csv')


## ------------------------------------------------------------
contr_location_te <- emmeans(fit_te, ~ location) |>
  pairs() |> confint() |>
  rename(Contrast = contrast,
         Estimate = estimate) |>
  mutate(CI = paste('(', round(lower.CL, 2), 
                          ', ', round(upper.CL, 2), ')')) |>
  dplyr::select(Contrast, Estimate, SE, CI)

contr_exposure_te <- emmeans(fit_te, ~ treatment) |>
  pairs() |> confint() |>
  rename(Contrast = contrast,
         Estimate = estimate) |>
  mutate(CI = paste('(', round(lower.CL, 2), 
                          ', ', round(upper.CL, 2), ')')) |>
  dplyr::select(Contrast, Estimate, SE, CI)

bind_rows(contr_location_te, contr_exposure_te) |>
  write_csv(file = 'results/tbl/te-contrasts.csv')

## ------------------------------------------------------------
anova_te <- anova(fit_te, type = 'sequential')[-1,] 
rownames(anova_te) <- c('Location', 'Exposure',
                         'Hour', 'Day', 'Location:Exposure',
                         'Location:Hour', 'Exposure:Hour',
                         'Location:Day', 'Exposure:Day',
                         'Hour:Day', 'Location:Exposure:Hour',
                         'Location:Exposure:Day',
                         'Location:Hour:Day',
                         'Exposure:Hour:Day',
                         'Location:Exposure:Hour:Day')

anova_te |>
  rownames_to_column(var = 'Term') |>
  write_csv(file = 'results/tbl/te-anova.csv')
