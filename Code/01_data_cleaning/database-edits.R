## ASSP Mistnet Database Edits ##
## Amelia DuVall (ajduvall@uw.edu)
## 24 August 2020

library(here)
library(lubridate)
library(anchors)
library(tidyverse)

# Read-in data from earlier version
captures <- read.csv(here("Data", "captures.csv"))
cpue <- read.csv(here("Data", "cpue.csv")) 

# Create new captures sheet
captures.v2 <- captures %>%
  replace.value(c("site_code"), from = "ND", to = "SR") %>% # fix ND value in site_code
  mutate(capture_time = as_datetime(capture_time, format = "%m/%d/%Y %H:%M")) %>%
  rename(capture_date = capture_time) %>%
  mutate(release_time = as_datetime(release_time, format = "%m/%d/%Y %H:%M")) %>%
  rename(release_date = release_time) %>%
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>% 
  mutate(capture_year = year(capture_date)) %>%
  mutate(capture_month = ifelse(month(capture_date) > 9, paste(month(capture_date)), paste(0, month(capture_date), sep = ""))) %>%
  mutate(capture_day = ifelse(day(capture_date) > 9, paste(day(capture_date)), paste(0, day(capture_date), sep = ""))) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_ID = paste(session_date,island_code, site_code, sep = "_")) %>%
  group_by(session_ID) %>% 
  mutate(seq = 1:n(),
         catch_ID = if_else(seq<10, paste(session_ID, seq, sep = "_0"), paste(session_ID, seq, sep = "_"))) %>% 
  ungroup() %>% 
  dplyr::select(catch_ID, session_ID, session_date, session_year, session_month, session_day, 
                island_code:capture_date, capture_year, capture_month, capture_day, capture_hour:notes)

# Create new CPUE sheet
cpue.v2 <- cpue %>% 
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_ID = paste(session_date,island_code, site_code, sep = "_")) %>%
  dplyr::select(session_ID:long, session_date, session_year, session_month, session_day, 
                series_ID:flagged_notes)

# Create session sheet
sessions <- cpue.v2 %>%
  dplyr::select(session_ID:series_ID,net_open_1:net_close_5, net_mesh:flagged_notes)

# Export out
write.csv(captures.v2, here("Working", "captures.csv"))
write.csv(cpue.v2, here("Working", "cpue.csv"))
write.csv(sessions, here("Working", "sessions.csv"))
