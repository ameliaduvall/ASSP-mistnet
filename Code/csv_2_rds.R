## Script used to export csv files to RDS
library(easypackages)
libraries("here", "tidyverse", "lubridate")

# Read in data
sessions.raw <- read.csv(here("Working", "sessions.csv"), na.strings = c("ND", "NA"))
captures.raw <- read.csv(here("Working", "captures.csv"), na.string = c("ND", "NA"))

# Fix day/times
captures <- captures.raw %>%
  mutate(session_date = as_date(session_date, format = "%m/%d/%Y")) %>%
  mutate(capture_date = as_datetime(capture_date, format = "%m/%d/%Y %H:%M")) %>%
  mutate(release_date = as_datetime(release_date, format = "%m/%d/%Y %H:%M")) 

sessions <- sessions.raw %>%
  mutate(session_date = as_date(session_date, format = "%m/%d/%Y")) %>%
  mutate_at((c("net_open_1", "net_open_2", "net_open_3", "net_open_4", "net_open_5",
               "net_close_1", "net_close_2", "net_close_3", "net_close_4", "net_close_5")), 
            ~ as_datetime(.x, format = "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")) %>%
  select(-1)

# Export as RDS
saveRDS(sessions, here("Working", "sessions.RDS"))
saveRDS(captures, here("Working", "captures.RDS"))
