#### Thermal Ecology of Crotalus viridis ####
# Updated 2 February 2024 by H Moniz

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, RColorBrewer, readr, stringr, lubridate, rstatix, Rmisc, ggpubr, ggplot2)
# rstatix get_summary_stats, Rmisc summarySE

theme_set(theme_classic() +
            theme(text = element_text(color = "black", 
                                      family = "sans", 
                                      size = 16),
                  axis.text = element_text(color = "black", 
                                           family = "sans", 
                                           size = 12),
                  legend.text = element_text(color = "black", 
                                             family = "sans", 
                                             size = 12),
                  legend.text.align = 0,
                  legend.position = "bottom"
            ))


#### Morphometrics of snakes from sites ####
 
Morpho <- read.csv("data/Cviridis_Morphometrics.csv")

Morpho <- Morpho %>%
  mutate(Repro = ifelse(grepl("NP*", SnakeID),
                        "Non-Pregnant", "Pregnant"))

ggplot(Morpho, aes(Repro, SVL_mm, color = Repro)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9)

ggplot(Morpho, aes(Repro, Mass_g, color = Repro)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9)

t.test(SVL_mm ~ Repro, data = Morpho)
# p = 0.367, mean Pregnant 871.25, mean Non-Pregnant 844.70
t.test(Mass_g ~ Repro, data = Morpho)
# p = 0.00014, mean Pregnant 568.925, mean Pregnant 393.13



#### Thermal Gradient ####
# To calculate thermal preferences which are used to later define environmental quality (de) and thermoregulatory efficiency (db)

TGradient <- read.csv("data/Cviridis_ThermalGradient.csv") %>%
  mutate(SnakeID = factor(SnakeID),
         Repro = factor(Repro))

TGradient %>%
  group_by(Repro, SnakeID) %>%
  get_summary_stats(Tb, show = c('q1', 'median', 'q3')) %>%
  write_csv('data/gradient-summary.csv')

#Summary statistics
TGradient %>%
  group_by(Repro) %>%
  get_summary_stats(Tb, show = c("min", "max", "mean", "median", "q1",
                                 "q3", "iqr"))

TG_Hour <- TGradient %>%
  mutate(Time =  sub(":.*", "", Time))

ggplot(TG_Hour, aes(Time, Tb)) +
  geom_boxplot(aes(fill = Repro))

#Subsetting
TGPregnant <- TGradient %>%
  filter(Repro == "Pregnant")

TGNonPregnant <- TGradient %>%
  filter(Repro == "Non-Pregnant")

hist(TGPregnant$Tb)
hist(TGNonPregnant$Tb)

var.test(Tb ~ Repro, data = TGradient)
# significant difference in variance between groups, F/ratio = 1.542

ggplot(TGradient, aes(x = SnakeID, y = Tb, fill = Repro))+
  geom_boxplot()+
  facet_wrap(~Repro, scale="free_x")+
  theme(text = element_text(size = 20), legend.position="none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Calculating basic summary stats for Tbs of all snakes:
TG_tbl <- TGradient %>%
  group_by(SnakeID) %>%
  get_summary_stats(Tb, show=c("min", "max", "mean", "median", "q1",
                               "q3", "iqr"))

# Pull mean and sd of median, interquartile 1, and interquartile 3 from this table for Tpref of all snakes

PregTG_Tbl <- TGPregnant %>%
  group_by(SnakeID) %>%
  get_summary_stats(Tb, show = c("min", "max", "mean", "median", "q1",
                                 "q3", "iqr"))

NonTG_Tbl <- TGNonPregnant %>%
  group_by(SnakeID) %>%
  get_summary_stats(Tb, show = c("min", "max", "mean", "median", "q1",
                                 "q3", "iqr"))

# Set range for preferred body temperature (Tpref) per repro group

PregLow <- mean(PregTG_Tbl$q1)
PregHigh <- mean(PregTG_Tbl$q3)

NonLow <- mean(NonTG_Tbl$q1)
NonHigh <- mean(NonTG_Tbl$q3)

shapiro.test(TGPregnant$Tb)
shapiro.test(TGNonPregnant$Tb)

ggqqplot(TGradient, "Tb", facet.by = "Repro")



#### NOAA Data to identify extreme low temps/storms during the season ####

NOAA <- read.csv("C:/Users/haley/OneDrive/Desktop/CalPoly/Projects/C. viridis/R datasets/Cviridis_NOAA_JuneOct2020.csv")
head(NOAA)
# plot DATE, SNOW, SNWD, TMIN_C, TMAX_C

NOAA <- NOAA %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

ggplot(data = NOAA, aes(x = DATE)) +
  geom_point(aes(y = TMIN_C), color = "dodgerblue2") +
  geom_line(aes(y = TMIN_C), color = "dodgerblue2") +
  geom_point(aes(y = TMAX_C), color = "red") +
  geom_line(aes(y = TMAX_C), color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) +
  scale_x_date(date_breaks = "1 day") +
  labs(title = "NOAA Max and Min Temps", subtitle = "June-Oct 2020") + xlab("Date") + ylab("Temp (C)")

# A few significant storms where max temps were < 20 C: June 9-11, July 1, Sept 9-12
# Remove dates with storms that wouldn't give them the opportunity to thermoregulate


#### OTM Data to identify extreme low temps ####

OTM <- read.csv("C:/Users/haley/OneDrive/Desktop/CalPoly/Projects/C. viridis/R datasets/Cviridis_OTM_wden_20Nov23.csv", header = TRUE)
# Rook shade: OTM 1,5,7; Rook exposed: OTM 2,6,8; Den exposed: OTM 3; Den shaded: OTM 4; Prairie shade: OTM 10,11,13,15; Prairie exposed: OTM 9,12,14,16,17
# Dens act as both rookeries and hibernacula, we wrap them into rookeries for the analysis

OTM <- OTM %>%
  mutate(Date = as.Date(DateTime, format = "%m/%d/%Y"))

OTM <- OTM %>%
  mutate(Month = lubridate::month(OTM$Date),
         MonthName = month.name[Month])

OTMlows <- OTM %>% subset(Temp < 5) %>%
  group_by(Month, Time)

unique(OTMlows$Date)


#### Body temperature lows ####

Tb <- list.files(path = "C:/Users/haley/OneDrive/Desktop/CalPoly/Projects/C. viridis/R datasets/For_R_Analysis_Amended", full.names = TRUE)%>%
  lapply(read_csv) %>%
  bind_rows

Tb <- Tb %>% 
  mutate(Date = as.Date(Tb$Date, format = "%m/%d/%Y"))

Tb <- Tb %>%
  mutate(Month = lubridate::month(Tb$Date))

hist(Tb$Temp)

TbLow5 <- Tb %>%
  subset(Temp < 5) %>%
  group_by(Month, Time) %>%
  arrange(Date)



#### Environmental Temperature ####
Env <- OTM %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Time = as.character(Time))

Env <- Env %>%
  mutate(Month = lubridate::month(Env$Date),
         MonthName = month.name[Month])

Env <- Env %>%
  mutate(Time = hm(Time)) %>%
  mutate(Time = as.numeric(Time, units = "hours")) %>%
  mutate(DayNight = ifelse(Time >= 9 & Time <= 17,
                           "Day", "Night"))

Env %>%
  gather(-Temp, -OTMID, -Site, -DateTime, -MonthName, -DayNight, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Temp)) +
  geom_jitter() +
  facet_wrap(~ var, scales = "free")

hist(Env$Temp)
hist(log(Env$Temp))
# More data for prairie (count = 16,767) than Rookery (count = 14,904) - had more prairie sites

ggplot(Env, aes(factor(Time), Temp)) +
  geom_point(size = 0.6) +
  facet_wrap(~OTMID)

EnvShade <- Env %>%
  filter(Exposure == "Shaded")

ggplot(EnvShade, aes(Time, Temp)) +
  geom_point() +
  facet_wrap(~OTMID)

MeanEnv_Hour <- Env %>%
  group_by(Location, Exposure, Time) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))


# Fig 1A - Te by Time of Day [have previously had to reformat Location & Exposure as.factor() but okay now]

Fig1A <-
  ggplot(MeanEnv_Hour, aes(Time, mean, group = interaction(Location, Treatment), colour = Location)) +
  geom_point(size = 4, aes(shape = interaction(Location, Treatment))) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean - ci,
                    ymax = mean + ci), width = 0.3) + 
  annotate('rect', xmin = 0, xmax = 23, ymin = PregLow, ymax = PregHigh, alpha = 0.3, fill = 'royalblue3') +
  annotate('rect', xmin = 0, xmax = 23, ymin = NonLow, ymax = NonHigh, alpha = 0.3, fill = 'gold') +
  xlab("Time of Day (Hour)") + ylab("Mean Operative Model Temperature (\u00b0C)") +
  labs(colour = "Location", shape = "Exposure") +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2")) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = "Fig1A.jpg",
       plot = Fig1A,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 260, height = 100)


MeanEnv_DayNight <- Env %>%
  group_by(Location, Exposure, Date, Month, DayNight) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))


# Fig 1B. Te over dates by location and exposure - have had to as.factor(Location, Exposure, DayNight)
Fig1B <- 
  ggplot(MeanEnv_DayNight,
         aes(Date, mean, group = interaction(Location, Exposure), colour = Location)) +
  geom_point(size = 4, aes(shape = interaction(Location, Exposure))) +
  stat_smooth(se = TRUE, alpha = 0.3) +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-08-21"), ymin = PregLow, ymax = PregHigh, alpha=.2, fill='royalblue3') +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-08-21"), ymin = NonLow, ymax = NonHigh, alpha=.2, fill='gold') +
  facet_wrap(~DayNight) + # or by location
  xlab("Date") + ylab("Mean Operative Model Temperature (\u00b0C)") +
  labs(title = "Fig1B. Te by Date", colour = "Location", shape = "Exposure") +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2")) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = "Fig1B.jpg",
       plot = Fig1B,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 280, height = 240)



#### Field Active Body Temperatures (Tselect) ####

Tsel <- Tb %>%
  mutate(Time = (as.numeric(Time, units = "hours")/60/60)) %>%
  mutate(across(Time, round, 0)) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y")) %>%
  mutate(Date = ifelse(Time == 24, Date + lubridate::days(1), 
                       as.Date(Date))) %>%
  mutate(Date = as.Date.numeric(Date, origin = "1970-01-01")) %>%
  mutate(Time = case_when(Time == "24" ~ 0, 
                          TRUE ~ Time))

Tsel <- Tsel %>%
  mutate(MonthName = month.name[Month],
         Repro = ifelse(grepl("NG", SnakeID),
                        "Non-Pregnant", "Pregnant")) %>%
  mutate(DateTime = as.POSIXct(paste(as.character(Date), as.character(Time), sep = " "), format = "%Y-%m-%d %H")) %>%
  mutate(DayNight = ifelse(Time >= 9 & Time <= 17,
                           "Day", "Night"))

Tsel <- Tsel %>%
  filter(!(Month == 5 | Month == 9))


# Removing alternate locations because of low data availability (e.g. removing data where NP snakes were in rookeries and vice versa)

NonTsel <- Tsel %>%
  filter(str_detect(SnakeID, "NG")) %>%
  filter(!(Location == "Rookery"))

PregTsel <- Tsel %>%
  filter(!(str_detect(SnakeID, "NG"))) %>%
  filter(Location == "Rookery")

# Recombine
Tsel <- full_join(NonTsel, PregTsel)

Tsel %>%
  gather(-Temp, -SnakeID, -Month, -MonthName, -DateTime, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Temp, color = SnakeID)) +
  geom_jitter() +
  facet_wrap(~ var, scales = "free")

# more pregnant snake data (count = 14,090) than non-pregnant(12,798) driven by death of NG2 and NG4 during season

ggplot(Tsel, aes(x = Temp, fill = Repro)) +
  geom_histogram(alpha = 0.5)

ggplot(Tsel, aes(factor(Time), Temp, colour = Date, group = interaction(SnakeID, Date))) +
  geom_path() +
  facet_wrap(~SnakeID)

ggplot(Tsel, aes(Date, Temp, colour = Repro, group = interaction(Repro, Date))) +
  geom_path(position = position_dodge(0.4), size = 1)

Tsel %>%
  group_by(SnakeID) %>%
  get_summary_stats(Temp, show = c("min", "max", "mean"))

ggplot(Tsel, aes(x = factor(Time), y = Temp, colour = Repro)) +
  geom_boxplot() +
  facet_wrap(~Month)
  labs(y = "Temp (C)", x = "Time", title = "Tb by month") +
  annotate('rect', xmin = 0, xmax = 24, ymin = PregLow, ymax = PregHigh, alpha=.2, fill='royalblue3') +
  annotate('rect', xmin = 0, xmax = 24, ymin = NonLow, ymax = NonHigh, alpha=.2, fill='gold')

Mean_Tsel <- Tsel %>%
  group_by(Repro, Month, Time) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))

ggplot(na.omit(Mean_Tsel), aes(x = Time, y = mean, group = interaction(Month, Repro), color = as.factor(Month))) +
  geom_point(size = 2, aes(shape = Repro)) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci, width = 0.5)) +
  geom_path() + 
  labs(y = "Temp (C)", x = "Time", title = "Tb by month") +
  annotate('rect', xmin = 0, xmax = 24, ymin = PregLow, ymax = PregHigh, alpha=.2, fill='royalblue3') +
  annotate('rect', xmin = 0, xmax = 24, ymin = NonLow, ymax = NonHigh, alpha=.2, fill='gold')


MeanTsel_Hour <- Tsel %>%
  group_by(Repro, Time) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))

# Fig 3A. Tb by hour

Fig3A <-
  ggplot(MeanTsel_Hour, aes(Time, mean, colour = Repro)) +
  geom_point(size = 3, aes(shape = Repro)) +
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2")) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.3, linewidth = 1) + 
  xlab("Time of Day (Hour)") + ylab("Mean Field Active Body Temperature (\u00b0C)") +
  annotate('rect', xmin = 0, xmax = 24, ymin = PregLow, ymax = PregHigh, alpha = 0.2, fill='royalblue3') +
  annotate('rect', xmin = 0, xmax = 24, ymin = NonLow, ymax = NonHigh, alpha = 0.2, fill='gold') +
  theme(
    legend.position = c(0.8, 0.03),
    legend.justification = c("center", "bottom"),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = "Fig3A.jpg",
       plot = Fig3A,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 280, height = 200)

MeanTsel_DayNight <- Tsel %>%
  group_by(Repro, Date, DayNight) %>%
  get_summary_stats(Temp, show = c("mean", "ci"))


#Fig 3B. Tb by date

Fig3B <-
  ggplot(MeanTsel_DayNight, aes(Date, mean, group = interaction(Repro, DayNight), colour = interaction(Repro, DayNight))) +
  geom_point(size = 4, aes(shape = interaction(Repro, DayNight))) +
  geom_smooth() +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-09-01"), ymin = PregLow, ymax = PregHigh, alpha = 0.2, fill = 'royalblue3') +
  annotate('rect', xmin = as.Date("2020-6-01"), xmax = as.Date("2020-09-01"), ymin = NonLow, ymax = NonHigh, alpha = 0.2, fill = 'gold') +
  xlab("Date") + ylab("Mean Field Active Body Temp (\u00b0C)") +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2", "slateblue3", "gold2")) +
  theme(
    legend.position = c(0.8, 0.03),
    legend.justification = c("center", "bottom"),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = "Fig3B.jpg",
       plot = Fig3B,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 280, height = 200)


#### De with Tset ####

Master_de <- Env

# Calculating directionally rather than absolute, so we can see positive and negative values of actual difference between Te and Tset
Master_deRookery <- subset(Master_de, Location == "Rookery")
Master_dePrairie <- subset(Master_de, Location == "Prairie")

# directional
Master_deRookery$de <- ifelse(Master_deRookery$Temp > PregHigh, Master_deRookery$Temp - PregHigh,
                               ifelse(Master_deRookery$Temp < PregLow, Master_deRookery$Temp - PregLow, FALSE))
Master_dePrairie$de <- ifelse(Master_dePrairie$Temp > NonHigh, Master_dePrairie$Temp - NonHigh,
                                ifelse(Master_dePrairie$Temp < NonLow, Master_dePrairie$Temp - NonLow, FALSE))
# merge back together
de <- full_join(Master_deRookery, Master_dePrairie)

de %>%
  gather(-de, -OTMID, -Site, -DateTime, -Month, -MonthName, -Temp, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = de)) +
  geom_jitter() +
  facet_wrap(~ var, scales = "free")

hist(de$de)

ggplot(de, aes(Date, de, colour = Exposure)) +
  geom_boxplot(aes(y = de, shape = DayNight)) +
  facet_wrap(~Location) +
  geom_hline(yintercept = 0)

ggplot(de, aes(factor(Time), de, colour = Exposure)) +
  geom_point(aes(shape = DayNight), size = 2.5)
  facet_wrap(~Location) +
  facet_wrap(~Month) +
  geom_hline(yintercept = 0)

# Plotting means
de_Hour <- de %>%
  group_by(Time, Exposure, Location) %>%
  get_summary_stats(de, show = c("mean", "ci"))

# Fig2A. de by Hour, June through August - Prairie Te to NP Tpref, Rook Te to P Tpref
Fig2A <- 
  ggplot(de_Hour, aes(Time, mean, colour = Location, group = interaction(Location, Exposure))) +
  geom_point(size = 4, aes(shape = interaction(Location, Exposure))) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.3) +
  geom_path(linewidth = 1) +
  geom_hline(yintercept = 0) +
  labs(colour = "Location", shape = "Exposure") +
  xlab("Time of Day (Hour)") + ylab("Mean Thermal Quality (de)") +
  annotate(geom = "text", x = 1.5, y = 0.8, label = "Perfect Thermal Quality", size = 5) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2")) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


ggsave(filename = "Fig2A.jpg",
       plot = Fig2A,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 280, height = 200)


de_Month <- de %>%
  group_by(Exposure, Location, Date, DayNight) %>%
  get_summary_stats(de, show = c("mean", "ci"))


# Fig 2B. Fig2B. de by Date - Prairie Te to NP Tpref, Rook Te to p Tpref
Fig2B <- 
  ggplot(de_Month, aes(Date, mean, group = interaction(Location, Exposure, DayNight), colour = interaction(Location, Exposure))) +
  geom_point(size = 3, aes(shape = interaction(Location, Exposure))) +
  geom_smooth(SE = TRUE) +
  xlab("Date") + ylab("Mean Thermal Quality (de)") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2", "slateblue3", "gold2")) +
  facet_wrap(~DayNight) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

detag <- c("", "Perfect Thermal Quality")
Fig2B <-  tag_facet(Fig2B, 
                    x = as.Date("2020-06-18"), y = 2.7,
                    open = "", close = "",
                    size = 5,
                    family = "sans",
                    fontface = 1,
                    tag_pool = detag)

ggsave(filename = "Fig2B.jpg",
       plot = Fig2B,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 280, height = 180)


# Inset

de_Inset <- de %>%
  group_by(Exposure, Location, DayNight) %>%
  get_summary_stats(de, show = c("mean", "ci", "se"))

deInset <- 
  ggplot(de_Inset, aes(Exposure, mean, group = interaction(Location, Exposure), colour = interaction(Location, Exposure))) +
  geom_point(size = 4.5, stroke = 1, aes(shape = interaction(Location, Exposure))) +
  #geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.05, linewidth = 1) +
  facet_wrap(~DayNight, strip.position = "top") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2", "slateblue3", "gold2")) +
  ylab("Mean Thermal Quality (de)") +
  xlab("") +
  theme(legend.position = "none")

ggsave(filename = "deInset.jpg",
       plot = deInset,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 120, height = 120)



#### Db with Tset ####

Master_db <- Tsel

Master_dbPreg <- subset(Master_db, Repro == "Pregnant")
Master_dbNon <- subset(Master_db, Repro == "Non-Pregnant")

# directional
Master_dbPreg$db <- ifelse(Master_dbPreg$Temp > PregHigh, Master_dbPreg$Temp - PregHigh,
                           ifelse(Master_dbPreg$Temp < PregLow, Master_dbPreg$Temp - PregLow, FALSE))
Master_dbNon$db <- ifelse(Master_dbNon$Temp > NonHigh, Master_dbNon$Temp - NonHigh,
                              ifelse(Master_dbNon$Temp < NonLow, Master_dbNon$Temp - NonLow, FALSE))
# merge back together
db <- full_join(Master_dbPreg, Master_dbNon)

Lowdb <- arrange(db, db)
View(Lowdb)

db %>%
  gather(-db, -Temp, -Mass_g, -SnakeID, -DateTime, -Month, -MonthName, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = db, color = SnakeID)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

ggplot(db, aes(x = db, fill = Repro)) +
  geom_histogram(alpha = 0.5)

# no location because it should now be redundant with repro

db_Hour <- db %>%
  group_by(Repro, Time) %>%
  get_summary_stats(db, show = c("mean", "ci"))

# Fig 4A. db by Hour, June through August - "Non-Pregnant Tb to NP Tpref, Pregnant Tsel to P Tpref
Fig4A <-
  ggplot(db_Hour, aes(Time, mean, colour = Repro, group = Repro)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 4, aes(shape = Repro)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.3, linewidth = 1) +
  geom_path(linewidth = 1) +
  xlab("Time of Day (Hour)") + ylab("Mean Thermoregulatory Accuracy (db)") +
  annotate(geom = "text", x = 2.5, y = 0.2, label = "Perfect Thermoregulatory Accuracy", size = 5) +
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2")) +
  theme(
    legend.position = c(0.8, 0.02),
    legend.justification = c("center", "bottom"),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(filename = "Fig4A.jpg",
       plot = Fig4A,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 300, height = 200)


db_Month <- db %>%
  group_by(Repro, DayNight, Date) %>%
  get_summary_stats(db, show = c("mean", "ci"))


# Fig 4B. Fig4B. Db by Date - Non-Pregnant Tsel to NP Tpref, Pregnant Tsel to P Tpref

Fig4B <-
  ggplot(db_Month, aes(Date, mean, group = interaction(Repro, DayNight), colour = interaction(Repro, DayNight))) +
  geom_point(size = 3, aes(shape = interaction(Repro, DayNight))) +
  geom_smooth(SE = TRUE) +
  xlab("Date") + ylab("Mean Thermoregulatory Accuracy (db)") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 0, 16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2", "slateblue3", "gold2")) +
  #facet_wrap(~DayNight) +
  theme(
    legend.position = c(0.09, 0.01),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
# remove September label

dbtag <- c("", "Perfect Thermoregulatory Accuracy")
Fig4B <-tag_facet(Fig4B, 
                  x = as.Date("2020-06-05"), y = 1,
                  open = "", close = "",
                  size = 5,
                  family = "sans",
                  fontface = 1,
                  tag_pool = dbtag)

ggsave(filename = "Fig4B.jpg",
       plot = Fig4B,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 300, height = 180)


# Inset
db_Inset <- db %>%
  group_by(Repro, DayNight) %>%
  get_summary_stats(db, show = c("mean", "ci"))

dbInset <-
  ggplot(db_Inset, aes(DayNight, mean, colour = Repro)) +
  geom_point(size  = 4.5, stroke = 1, aes(shape = Repro)) +
  #  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.05, linewidth = 1) +
  #facet_wrap(~DayNight, strip.position = "top") +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("slateblue3", "gold2", "slateblue3", "gold2")) +
  ylab("Mean Thermoregulatory Accuracy (db)")+
  xlab("") +
  theme(legend.position = "none")
# alt, use Repro as x-axis and facet_wrap by DayNight

ggsave(filename = "dbInset.jpg",
       plot = dbInset,
       device = "jpg",
       dpi = 600,
       units = "mm",
       width = 120, height = 120)


#### Notes to Trevor ####
# We'd previously been removing the major storm in June after truncating the "active season" to June-Aug. I'm not sure this is the most appropriate approach though