# Thermal Ecology of pregnant and non-pregnant Prairie Rattlesnakes in two habitat types
# Environmental loggers deployed in exposed and shaded conditions in both habitat types (n = 14 paired exposed/shaded OTMs, 1 unpaired) - From Jun 1 to Aug 21
# Internal thermal loggers within 7 pregnant and 7 non-pregnant rattlesnakes - From Jun 1 to Aug 31
# Pregnant snakes primarily occupied "Rookery" sites and non-pregnant snakes mostly occupied "Prairie" sites

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, stringr, lubridate, rstatix, Rmisc, ggplot2)

# There are four main variables to model
# Te: Environmental temperature
# Tb: Body temperatures of snakes in the wild
# de: Habitat thermal quality: the difference between a lab-assigned preferred temperature (Tpref) and Te
# db: Thermoregulatory accuracy - the difference between Tb and Tpref

# Te from Operative thermal models (OTMs - environmental loggers)
path <- "DataforTrevor/OTM_wden_20Nov23.csv"
OTM <- read.csv(path, header = TRUE)
# Rookery shade: OTM 1,3,5,7; Rookery exposed: OTM 2,4,6,8; Prairie shade: OTM 10,11,13,15; Prairie exposed: OTM 9,12,14,16,17

OTM <- OTM %>%
  mutate(date = as.Date(OTM$DateTime, format = "%m/%d/%Y"))

OTM <- OTM %>%
  mutate(month = lubridate::month(OTM$date),
         month_name = month.name[lubridate::month(OTM$date)])

Env <- OTM

Env <- Env %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Time = as.character(Time))

Env <- Env %>%
  mutate(month = lubridate::month(Env$Date),
         month_name = month.name[lubridate::month(Env$Date)])

Env <- Env %>%
  mutate(time = hm(Time)) %>%
  mutate(time = as.numeric(time, units = "hours"))

# Remove some extreme lows from a June storm
Env <- Env %>%
  filter(!(date == '2020-06-09' | date == '2020-06-10' | date == '2020-06-11' | date == '2020-06-12'))

# Assigning a categorical variable for times when snakes can actively thermoregulate (9am - 5pm) versus when they cannot (5pm - 8am)
Env <- Env %>%
  mutate(Active = ifelse(Time_Int == 9 | Time_Int == 10 | Time_Int == 11 | Time_Int == 12 | Time_Int == 13 | Time_Int == 14 | Time_Int == 15 | Time_Int == 16 | Time_Int == 17,
                         "Active", "Inactive"))


# Removing a few days in May when loggers were deployed so we just have June-Aug
Env <- Env %>%
  filter(!(month == "5"))

meanEnv_hour <- Env %>%
  group_by(Location, Treatment, time) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))


# Fig 1A - Te over time of day for active season (Jun-Aug) without confidence intervals. Blue bar is the bounds around mean of median Tpref for non-pregnant snakes which is higher than Tpref of pregnant snakes (red bar)
ggplot(meanEnv_hour, aes(time, mean, group = interaction(as.factor(Location), as.factor(Treatment)), colour = as.factor(Treatment))) +
  geom_point(aes(shape = as.factor(Location), size = 1.3)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Time of Day (Hour)") + ylab("Temp (C)") +
  labs(title = "Fig1A. Te by time of day") +
  annotate('rect', xmin = 0, xmax = 24, ymin = 29.1, ymax = 31.9, alpha=.2, fill='red') +
  annotate('rect', xmin = 0, xmax = 24, ymin = 30.6, ymax = 33.45, alpha=.2, fill='blue')

ggsave(filename = 'img/from-haley-fig1a.png', width = 8, height = 4, units = 'in')

meanEnv_active <- Env %>%
  group_by(Location, Treatment, date, month, Active) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))


# Fig 1B. Te over dates by location and exposure - red is Tpref of pregnant snakes, blue is Tpref of non-pregnant snakes. Split into the equivalent of daytime thermoregulatory hours (active) and nighttime hours when snakes can't thermoregulation (inactive).
ggplot(meanEnv_active, aes(date, mean, group = interaction(as.factor(Location), as.factor(Treatment)), colour = interaction(as.factor(Location), as.factor(Treatment)))) +
  geom_point(aes(shape = interaction(as.factor(Location), as.factor(Treatment)), size = 1.3)) +
  geom_smooth(aes(fill = interaction(as.factor(Location), as.factor(Treatment)))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Date") + ylab("Temp (C)") +
  labs(title = "Fig1B. Te by Date") +
  facet_wrap(~Active) + 
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-08-21"), ymin = 29.1, ymax = 31.9, alpha=.2, fill='red') +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-08-21"), ymin = 30.6, ymax = 33.45, alpha=.2, fill='blue')

ggsave(filename = 'img/from-haley-fig1b.png', width = 12, height = 4, units = 'in')


# Big takeaway: Rookeries at night buffer snakes from extreme lows



# Tb from thermal loggers implanted into 7 pregnant and 7 non-pregnant snakes. Data is simplified so that location and pregnancy status are redundant (i.e., pregnant snakes are only ever at rookeries, and non-pregnant only in the prairie)

Tsel <- list.files(path = "DataforTrevor/For_R_Analysis_Amended", full.names = TRUE)%>%
  lapply(read_csv) %>%
  bind_rows

Tsel2 <- Tsel %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

Tsel2 <- Tsel2 %>%
  mutate(month = lubridate::month(Tsel2$date),
         month_name = month.name[lubridate::month(Tsel2$date)],
         repro = ifelse(grepl("NG", snakeid),
                        "Nongravid", "Gravid"))

# Return - clean up with tidyr - ignore my mess!
Tsel2$time<-as.numeric(Tsel2$time, units= "hours")
Tsel2$time<-Tsel2$time/60/60
Tsel2$time<-round(Tsel2$time, digits = 0)
Tsel2$date <- as.Date(Tsel2$date, "%m/%d/%y")
Tsel2$date<-ifelse(Tsel2$time==24, Tsel2$date+lubridate::days(1), 
                   as.Date(Tsel2$date))
Tsel2$date <- as.Date(as.numeric(Tsel2$date), origin = "1970-01-01")
Tsel2$time[Tsel2$time==24]<-0

# Remove extreme lows from June storm
Tsel <- Tsel2 %>%
  filter(!(date == '2020-06-09' | date == '2020-06-10' | date == '2020-06-11' | date == '2020-06-12'))

Tsel <- Tsel %>%
  mutate(datetime = as.POSIXct(paste(as.character(date), as.character(time), sep = " "), format = "%Y-%m-%d %H"))


# Removing alternate locations because of low data availability (e.g. removing data where NG snakes were in rookeries and vice versa)

NGTsel <- Tsel %>%
  filter(str_detect(snakeid, "NG")) %>%
  filter(!(location == "rook"))

GTsel <- Tsel %>%
  filter(!(str_detect(snakeid, "NG"))) %>%
  filter(location == "rook")

# Recombine
Tsel <- full_join(NGTsel, GTsel)

# Creative an "active" and "inactive" categorical variable for thermoregulatory opportunities
Tsel <- Tsel %>%
  mutate(Active = ifelse(time == 9 | time == 10 | time == 11 | time == 12 | time == 13 | time == 14 | time == 15 | time == 16 | time == 17,
                         "Active", "Inactive"))


# shortening dataset to active season of June-Aug
Tsel <- Tsel %>%
  filter(!(month == 5 | month == 9))


meanTsel_hour <- Tsel %>%
  group_by(repro, time) %>%
  get_summary_stats(temp, show = c("mean", "ci"))

# Fig 3A. Tb by hour for all days of the active season (Jun-Aug). Blue is the interquartile bounds of mean of median Tpref for non-pregnant snakes, and red is for pregnant snakes.
ggplot(meanTsel_hour, aes(time, mean, colour = as.factor(repro))) +
  geom_point(aes(shape = as.factor(repro), size = 1.3)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Time of Day (Hour)") + ylab("Field Active Temp (C)") +
  labs(title = "Fig3A. Tb by time of day") +
  annotate('rect', xmin = 0, xmax = 24, ymin = 29.1, ymax = 31.9, alpha=.2, fill='red') +
  annotate('rect', xmin = 0, xmax = 24, ymin = 30.6, ymax = 33.45, alpha=.2, fill='blue')

ggsave(filename = 'img/from-haley-fig3a.png', width = 8, height = 4, units = 'in')

meanTsel_Active <- Tsel %>%
  group_by(repro, date, Active) %>%
  get_summary_stats(temp, show = c("mean", "ci"))

#Fig 3B. Tb by date, 
ggplot(meanTsel_Active, aes(date, mean, group = interaction(as.factor(repro), as.factor(Active)), colour = as.factor(repro))) +
  geom_path(aes(shape = as.factor(Active))) +
  geom_smooth(aes(fill = repro)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Date") + ylab("Temp (C)") +
  labs(title = "Fig3B. Te by Date") +
  facet_wrap(~Active) + 
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-09-01"), ymin = 29.1, ymax = 31.9, alpha=.2, fill='red') +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-09-01"), ymin = 30.6, ymax = 33.45, alpha=.2, fill='blue')

ggsave(filename = 'img/from-haley-fig3b.png', width = 12, height = 4, units = 'in')


#### De with Tpref ####

# Hertz et al. (1977) established a way of measuring how well a habitat thermally suits an organism, through the absolute value of the difference between Te and Tpref. We don't use absolute value, and instead calculate it directionally. This leads to a negative skew that I'm unsure how best to model.

masterDe <- Env
# Cutting De and Db to June-August

masterDe <- masterDe %>%
  filter(!(month == 5))

# First and third interquartiles of Tpref calculated from a lab thermal gradient for pregnant (G) and nonpregnant (NG) snakes
Glow <- 29.1
Ghigh <- 31.9

NGlow <- 30.6
NGhigh <- 33.45


# directional by repro group

masterDe_Rook <- subset(masterDe, Location == "Rookery")
masterDe_Prairie <- subset(masterDe, Location == "Prairie")

# directional
masterDe_Rook$De <- ifelse(masterDe_Rook$Temp > Ghigh, masterDe_Rook$Temp - Ghigh,
                           ifelse(masterDe_Rook$Temp < Glow, masterDe_Rook$Temp - Glow, FALSE))
masterDe_Prairie$De <- ifelse(masterDe_Prairie$Temp > NGhigh, masterDe_Prairie$Temp - NGhigh,
                              ifelse(masterDe_Prairie$Temp < NGlow, masterDe_Prairie$Temp - NGlow, FALSE))

# merge back together
masterDe <- full_join(masterDe_Rook, masterDe_Prairie)

De_hour <- masterDe %>%
  group_by(time, Treatment, Location) %>%
  get_summary_stats(De, show = c("mean", "ci"))


# Fig2A. De by hour. 0 on this graph represents the ideal, with no difference between Tpref and Te. Again, it's the buffering effect of rookeries in overnight hours that benefits snakes in this habitat.

ggplot(De_hour, aes(factor(time), mean, colour = Treatment, group = interaction(Location, Treatment))) +
  geom_point(aes(shape = Location), size = 2.5) +
  #  geom_errorbar(ymin = De - ci, ymax = De + ci) +
  geom_path() +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +
  labs(title = "De by Hour, June through August", subtitle = "Prairie Te to NG Tpref, Rook Te to G Tpref")

ggsave(filename = 'img/from-haley-fig2a.png', width = 8, height = 4, units = 'in')

De_month <- masterDe %>%
  group_by(Treatment, Location, date, Active) %>%
  get_summary_stats(De, show = c("mean", "ci"))

# Fig 2B. De by date
ggplot(De_month, aes(date, mean, group = interaction(as.factor(Location), as.factor(Treatment), as.factor(Active)), colour = interaction(as.factor(Location), as.factor(Treatment)))) +
  geom_point(aes(shape = as.factor(Active))) +
  geom_smooth(aes(fill = interaction(as.factor(Location), as.factor(Treatment)))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Date") + ylab("Temp (C)") +
  geom_hline(yintercept = 0) +
  labs(title = "Fig2B. De by Date", subtitle = "Prairie Te to NG Tpref, Rook Te to G Tpref") +
  facet_wrap(~Active)

ggsave(filename = 'img/from-haley-fig2b.png', width = 12, height = 4, units = 'in')

#### Db with Tset ####
# Db is the difference instead between a snake's body temperature in the field and its Tpref, or its ability to reach Tpref.

masterDb <- Tsel

masterDb_G <- subset(masterDb, repro == "Gravid")
masterDb_NG <- subset(masterDb, repro == "Nongravid")


masterDb_G$Db <- ifelse(masterDb_G$temp > Ghigh, masterDb_G$temp - Ghigh,
                        ifelse(masterDb_G$temp < Glow, masterDb_G$temp - Glow, FALSE))
masterDb_NG$Db <- ifelse(masterDb_NG$temp > NGhigh, masterDb_NG$temp - NGhigh,
                         ifelse(masterDb_NG$temp < NGlow, masterDb_NG$temp - NGlow, FALSE))

# merge back together
masterDb <- full_join(masterDb_G, masterDb_NG)

# no location because it should now be redundant with pregnant/non-pregnant

Db_hour <- masterDb %>%
  group_by(repro, time) %>%
  get_summary_stats(Db, show = c("mean", "ci"))


# Fig 4A. - Db by hour of day
ggplot(Db_hour, aes(factor(time), mean, colour = repro, group = repro)) +
  geom_point(aes(shape = repro), size = 2.5) +
  #  geom_errorbar(ymin = De - ci, ymax = De + ci) +
  geom_path() +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +
  labs(title = "Fig 4A. Db by Hour, June through August", subtitle = "NG Tb to NG Tpref, Gravid Tb to G Tpref")

ggsave(filename = 'img/from-haley-fig4a.png', width = 8, height = 4, units = 'in')

Db_month <- masterDb %>%
  group_by(repro, Active, date) %>%
  get_summary_stats(Db, show = c("mean", "ci"))

# Fig 4B. Db by date
ggplot(Db_month, aes(date, mean, group = interaction(as.factor(repro), as.factor(Active)), colour = interaction(as.factor(repro), as.factor(Active)))) +
  geom_point(aes(shape = as.factor(Active))) +
  geom_path() +
  #geom_smooth(aes(fill = interaction(as.factor(repro), as.factor(Active)))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Date") + ylab("Temp (C)") +
  geom_hline(yintercept = 0) +
  labs(title = "Fig4B. Db by Date", subtitle = "Nongravid Tb to NG Tpref, Gravid Tb to G Tpref")

ggsave(filename = 'img/from-haley-fig4b.png', width = 8, height = 4, units = 'in')

### Model approach ###

# Obstacles: negative-skewed data

hist(masterDe$De)
hist(masterDb$Db)

# Over-saturation or too much data? 24 hours a day, 79 days for 15 OTMs or 14 snakes, depending on variable of interest

# Correlations: Cyclically timed and within-individual observations


# General model: Te/de ~ location * exposure + month + timeofday
#                Tb/db ~ reproductive_status + month + timeofday
# Random variable: OTM ID or snake ID, depending on variable