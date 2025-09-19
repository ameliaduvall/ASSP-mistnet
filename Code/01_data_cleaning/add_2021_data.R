## Add 2021 data from CIES to mistnetting database
## Amelia DuVall ajduvall@uw.edu
## 12 Oct 2021

## load libraries
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(lubridate)

## load database files
captures <- readRDS(here("Working", "captures.RDS"))
#cpue <- readRDS(here("Working", "cpue.RDS"))
sessions <- readRDS(here("Working", "sessions.RDS"))
sites <- read_csv(here("Working", "sites.csv")) %>%
  dplyr::select(site_code, lat, long)

## load 2021 data
bands2021 <- read_excel(here("data", "2021 ASSP Mistnetting.xls"), sheet = "CAPTURES") %>%
  clean_names()
sessions2021 <- read_excel(here("data", "2021 ASSP Mistnetting.xls"), sheet = "NET HOURS") %>%
  clean_names()

## reformat 2021 data
colnames(bands2021)
colnames(captures)

## add new columns to captures df
capturesnew <- captures %>%
  mutate(tail_fork = c(NA),
         bander_id = c(NA),
         how_sexed = c(NA),
         how_aged = c(NA),
         age = c(NA)) %>%
  dplyr::select("catch_ID", "session_ID", "session_date", "session_year", "session_month", "session_day", 
                "island_code", "subisland_code", "site_code", "lat", "long", "capture_date",
                "capture_year", "capture_month", "capture_day", "capture_hour", "capture_min", "release_date",
                "species", "band_no", "recapture", "replaced", "diet", "BP", "uncorr_mass", "mass_tare", "mass_corr", 
                "culmen", "skull_length", "tarsus", "wing", "tail", "tail_fork", "sex", "how_sexed", "age", "how_aged", 
                "bander_id", "measurers", "flagged", "notes")

bands2021new <- bands2021 %>%
  rename(band_no = band_number,
         site_code = location,
         capture_date = banding_date,
         mass_corr = bird_weight,
         culmen = culmen_length,
         tarsus = tarsus_length,
         wing = wing_chord,
         BP = brood_patch) %>%
  mutate(site_code = ifelse(site_code == "SRK", "SR", ifelse(site_code == "ESC", "ESP", site_code)),
         island_code = ifelse(site_code == "AP" | site_code == "ESP", "SBI",
                              "SCI"),
         subisland_code = ifelse(island_code == "SBI", "SBI", "SR"),
         capture_year = year(capture_date),
         capture_month = month(capture_date),
         capture_day = day(capture_date),
         capture_hour = hour(capture_time),
         capture_min = minute(capture_time),
         release_date = c(NA),
         recapture = ifelse(bander_id == "RECAP", "Y", "N"),
         replaced = c("N"),
         diet = c(NA),
         measurers = c(NA),
         uncorr_mass =c(NA),
         mass_tare = c(NA),
         skull_length = c(NA),
         tail = c(NA),
         flagged = c(NA),
         notes = c(NA),
         session_date = ifelse(capture_hour > 5, ymd(capture_date), ymd(capture_date) - days(1)),
         session_date = as_date(session_date),
         session_month = month(session_date),
         session_day = day(session_date),
         session_year = year(session_date),
         session_ID = paste(session_date, island_code, site_code, sep = "_"),
         bander_id = ifelse(bander_id == "RECAP", NA, bander_id),
         band_no = ifelse(band_no == "NO BAND", "notbanded", band_no)) %>%
  group_by(session_ID) %>% 
  mutate(seq = 1:n(),
         catch_ID = if_else(seq<10, paste(session_ID, seq, sep = "_0"), paste(session_ID, seq, sep = "_"))) %>% 
  ungroup() %>% 
  left_join(sites, by = "site_code") %>%
  dplyr::select(colnames(capturesnew))

## join captures dataframes
captures_v2 <- rbind(capturesnew, bands2021new)

## export out
write.csv(captures_v2, here("data", "database", "captures.csv"), row.names = FALSE)
saveRDS(captures_v2, here("data", "database", "captures.RDS"))

### format sessions data
colnames(sessions)
colnames(sessions2021)

sessions2021new <- sessions2021 %>%
  rename(session_date = date,
         site_code = location,
         net_open_1 = net_open,
         net_close_1 = net_close) %>%
  mutate(site_code = ifelse(site_code == "SRK", "SR", ifelse(site_code == "ESC", "ESP", site_code)),
         island_code = ifelse(site_code == "AP" | site_code == "ESP", "SBI", "SCI"),
         subisland_code = ifelse(island_code == "SBI", "SBI", "SR"),
         net_open_2 = c(NA),
         net_close_2 = c(NA),
         net_open_3 = c(NA),
         net_close_3 = c(NA),
         net_open_4 = c(NA),
         net_close_4 = c(NA),
         net_open_5 = c(NA),
         net_close_5 = c(NA),
         net_mesh = c(NA),
         net_dim = c(NA),
         spp_audio_file = c("ASSP"),
         dB_level = c(NA),
         speaker_system = c(NA),
         org = c("Calif. Institute of Env'l Studies (CIES)/Channel Islands National Park (CHIS)"),
         notes = c(NA),
         flagged = c("N"),
         flagged_notes = c(NA)) %>%
  separate(col = net_open_1, into = c("net_open_1_hr", "net_open_1_min"), sep = 2) %>%
  unite("net_open_1", c("net_open_1_hr", "net_open_1_min"), sep = ":", remove = TRUE) %>%
  separate(col = net_close_1, into = c("net_close_1_hr", "net_close_1_min"), sep = 1) %>%
  unite("net_close_1", c("net_close_1_hr", "net_close_1_min"), sep = ":", remove = TRUE) %>%
  unite("net_open_1_drop", c("session_date", "net_open_1"), sep = " ", remove = FALSE) %>%
  mutate("session_date_+1" = ymd(session_date) + days(1)) %>%
  unite("net_close_1_drop", c("session_date_+1", "net_close_1"), sep = " ", remove = FALSE) %>%
  mutate(net_open_1 = as_datetime(net_open_1_drop, format = "%Y-%m-%d %H:%M", tz = "America/Los_Angeles"),
         net_close_1 = as_datetime(net_close_1_drop, format = "%Y-%m-%d %H:%M", tz = "America/Los_Angeles")) %>%
  mutate(session_year = year(session_date),
         session_month = month(session_date),
         session_day = day(session_date),
         session_ID = paste(session_date, island_code, site_code, sep = "_"),
         series_ID = rep(c(1,2), 12),
         site_name = ifelse(site_code == "AP", "Arch Point", ifelse(
           site_code == "ESC", "Elephant Seal Point", "Scorpion Rock"))) %>%
  left_join(sites, by = "site_code") %>%
  dplyr::select(colnames(sessions))

## join sessions dataframes
sessions_v2 <- rbind(sessions, sessions2021new)

## export out
write.csv(sessions_v2, here("data", "database", "sessions.csv"), row.names = FALSE)
saveRDS(sessions_v2, here("data", "database", "sessions.RDS"))

  