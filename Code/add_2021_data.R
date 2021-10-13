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
captures <- readRDS(here("data", "captures.RDS"))
cpue <- readRDS(here("data", "cpue.RDS"))
sessions <- readRDS(here("data", "sessions.RDS"))

## load 2021 data
bands2021 <- read_excel(here("data", "2021 ASSP Mistnetting.xls"), sheet = "CAPTURES") %>%
  clean_names()
sessions2021 <- read_excel(here("data", "2021 ASSP Mistnetting.xls"), sheet = "NET HOURS") %>%
  clean_names()

## reformat 2021 data
colnames(bands2021)
colnames(captures)

bands2021new <- bands2021 %>%
  rename(band_no = band_number,
         site_code = location,
         capture_date = banding_date,
         mass_corr = bird_weight,
         culmen = culmen_length,
         tarsus = tarsus_length,
         wing = wing_chord,
         BP = brood_patch) %>%
  mutate(site_code = ifelse(site_code == "SRK", "SR", site_code),
         island_code = ifelse(site_code == "AP" | site_code == "ESC", "SBI",
                              "SCI"),
         subisland_code = ifelse(island_code == "SBI", "SBI", "SR"),
         capture_year = year(capture_date),
         capture_month = month(capture_date),
         capture_day = day(capture_date),
         capture_hour = hour(capture_time),
         capture_min = minute(capture_time),
         release_date = c(NA),
         recapture = c(NA),
         replaced  =c (N),
         diet = c(NA),
         measurers = c(NA),
         uncorr_mass =c(NA),
         mass_tare = c(NA),
         flagged = c(NA),
         notes = c(NA)) %>%
  dplyr::select()


## add new columns to captures df
capturesnew <- captures %>%
  mutate(tail_fork = c(NA),
         bander_id = c(NA),
         how_sexed = c(NA),
         how_aged = c(NA),
         age = c(NA))

## join captures dataframes


### format sessions data
sessions2021new <- sessions2021 %>%
  