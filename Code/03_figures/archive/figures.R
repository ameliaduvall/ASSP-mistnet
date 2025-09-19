## Figures for NFWF ASSP Report
## 6 July 2020
## Amelia DuVall (ajduvall@uw.edu)

## Load libraries
library(easypackages)
libraries("tidyverse", "lubridate", "ggplot2", "readxl", "here", "wesanderson", "mosaic", "ggthemes")

## Read-in data and set-up for analysis
banding <- read_excel("CHIS_ASSP_mistnet_database_07092020_v2.xlsx", sheet = "Banding", 
                      col_names = TRUE, na = c("NA", "ND"))

cpue <- read_excel("CHIS_ASSP_mistnet_database_07092020_v2.xlsx", sheet = "CPUE", 
                   col_names = TRUE, na = c("NA", "ND"))

## Filter data to ASSP species and banded individuals only.
ASSP <- group_by(.data = banding) %>%
  filter(species == "ASSP") %>%
  filter(band_no != "notbanded") %>%
  ungroup()

## Morphometric Figures 
#### Wing chord measurements ####
summary(ASSP$wing)

# Histogram
ggplot(data = ASSP, aes(x = wing, na.rm = TRUE)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) + 
  geom_density(alpha = .5, fill = "gray") +
  geom_vline(aes(xintercept = mean(wing, na.rm = TRUE)),
             colour = "red", linetype ="longdash", size = .8) +
  theme_bw()

# Sorted values plot
plot(sort(ASSP$wing), ylab = "Sorted Wing Chord")

# Check for a bimodal distribution of the wing chord morphometric data. 
# A bimodal distribution could be an indication of different methods used to measure wing chord 
# (e.g, flattened wing chord versus relaxed wing chord). 

# How about mean wing chord observed per session?
WC_ses <- group_by(.data = ASSP, session_ID, year) %>%
  summarise(wing_av = mean(wing, na.rm = TRUE)) %>%
  ungroup()

ggplot(WC_ses) +
  geom_point(aes(x = session_ID, y = wing_av, color = year))  +
  theme_minimal()
# Values in earlier years (~1990s) appear smaller. 

# How about mean wing chord observed per year?
WC_yr <- group_by(.data = ASSP, year) %>%
  summarise(wing_av = mean(wing, na.rm = TRUE)) %>%
  ungroup()

ggplot(WC_yr) +
  geom_point(aes(x = year, y = wing_av))  +
  theme_minimal()

# Boxplot of wing chord values
avwing <- mean(ASSP$wing, na.rm = TRUE)

# by island
ggplot(ASSP, aes(x = as.factor(year), y = wing, fill = island_code)) +
  geom_boxplot() +
  xlab("Year") + ylab("Wing Chord (mm)") +
  geom_hline(yintercept = avwing, linetype = "dashed", color = "black", size = 2) +
  scale_fill_manual(values = wes_palette(4, name = "Darjeeling2", type = "continuous"), name = "") +
  coord_flip() +
  theme_minimal()


ggplot(ASSP, aes(x = as.factor(year), y = wing, fill = island_code)) +
  geom_boxplot() +
  xlab("Year") + ylab("Wing Chord (mm)") +
  geom_hline(yintercept = avwing, linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = wes_palette(4, name = "Darjeeling2", type = "continuous"), name = "") +
  coord_flip() +
  facet_wrap(~island_code) +
  theme_minimal()

# by organization
ASSPorg <- left_join(ASSP, cpue, by = "session_ID") %>% #  gain 11 sessions (w/o captures?)
  filter(!is.na(org)) %>%
  replace.value(ASSPorg, c("org"), from = "USGS WERC", to = "USGS-WERC")

## try inner_join

ggplot(ASSPorg, aes(x = as.factor(year.x), y = wing, fill = org)) +
  geom_boxplot() +
  xlab("Year") + ylab("Wing Chord (mm)") +
  scale_fill_manual(values = wes_palette(4, name = "Darjeeling2", type = "continuous"), name = "") +
  geom_hline(yintercept = avwing, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
  coord_flip() +
  theme_minimal()

org.sum <- ASSPorg %>%
  group_by(year.x, org) %>%
  summarize(mean_wing = mean(wing, na.rm = TRUE))

ggplot(ASSPorg, aes(x = org, y = wing, fill = org)) +
  geom_boxplot() +
  xlab("Organization") + ylab("Wing Chord (mm)") +
  scale_fill_manual(values = wes_palette(4, name = "Darjeeling2", type = "continuous"), name = "") +
  geom_hline(yintercept = avwing, linetype = "dashed", color = "black", size = 1, alpha = 0.5) +
  coord_flip() +
  theme_minimal()


#### Recaps ####
recap <- group_by(.data = ASSP, year, recapture) %>%
  summarise(no_captures = n()) %>%
  ungroup()
# check R value

ggplot(recap) +
  geom_col(aes(x = as.factor(year), y = no_captures, fill = recapture), position = "dodge") +
  ylab("Number of Captures") + xlab ("Year") +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  theme_minimal()

ggplot(recap) +
  geom_col(aes(x = as.factor(year), y = no_captures, fill = recapture), width = 0.8) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  ylab("Number of Captures") + xlab ("Year") +
  theme_minimal()

# use filled box plot with sample size at the top

#### Brood patch ####
summary(ASSP$BP)
unique(ASSP$BP)  

metadata <- cpue %>% 
  mutate_at(c("app_sunset", "std_ending"), 
            .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3", 
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>%
  mutate_at(c("app_sunset", "std_ending", "net_open_1", "net_close_1"), 
            .funs = list(time = ~ hms::as_hms(.))) %>% 
  mutate(CPUE_ratio = CPUEstd/CPUEraw, 
         month = as.character(month)) %>% 
  filter(TRUE)

metadata_effort <- metadata %>%
  dplyr::select(session_ID, site_code, app_sunset, std_ending, lat, long)

catches <- banding %>% 
  left_join(metadata_effort, by = c("session_ID", "site_code", "lat", "long")) %>%
  mutate_at(c("app_sunset", "std_ending", "capture_time", "release_time"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>%
  filter(species == "ASSP") %>%
  mutate(std = if_else(std_ending > capture_time, "1", "0"),
         catchPastSS = capture_time - app_sunset,
         assumeBreed = mosaic::derivedFactor(
           "Y" = (BP == "B" | BP == "b" | BP == "2" | BP == "3" | BP == "4"),
           "N" = (BP == "D" | BP == "d" | BP == "0" | BP == "5" | BP == "PD" | BP == "pd" | BP == "1" | BP == "1.5" | BP == "4.5"),
           .default = "ND")) %>%
  filter(TRUE)

metadata_BP <- catches %>%
  group_by(session_ID, site_code) %>%
  summarise(ASSP = n(),
            ASSPstd = sum(std == "1"),
            BPct = n(),
            BP_Y = sum(assumeBreed == "Y"), # number of birds that have a broodpatch (2-4, B)
            BP_N = sum(assumeBreed == "N")) %>% # number of birds that dont have a broodpatch (1, 1.5, 4.5, 5, PB, D)
  right_join(metadata, by= c("session_ID", "site_code")) %>%
  mutate(BPfreq_Y = BP_Y/BPct, # frequency of birds that have a broodpatch
         BPfreq_N = BP_N/BPct)

## Brood Patch and Assumed Breeders
monthCatches <- metadata %>%
  group_by(month) %>%
  tally()

ggplot(metadata_BP, aes(x = month, y = BPfreq_Y, fill = month)) +
  geom_boxplot() +
  ylab("Frequency of Assumed Breeders") +
  geom_text(data = monthCatches,
            aes(month, Inf, label = n), vjust = 1.2) +
  scale_fill_manual(values = wes_palette(6, name = "Darjeeling2", type = "continuous"), name = "") +
  theme_bw()

## split up by site or island 
## frequency of assumed breeders by time of night