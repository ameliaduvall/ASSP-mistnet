## ASSP Mistnet Session ID Check ##
## Amelia DuVall (ajduvall@uw.edu)
## 21 August 2020

# The purpose of this script is to check errors discovered in the session_ID field. 
# As I was working on the figures for the ASSP NFWF report, I've discovered that there were
# 11 mist-netting sessions that are not recorded in the CPUE sheet, 
# accounting for 112 banding records.
# Issue discovered 17 July 2020

library(here)
library(lubridate)
library(anchors)
library(tidyverse)

# Read-in data from earlier version (9 July 2020 v2) - prior to changes being made manually in database
captures <- read.csv(here("Data", "captures_07092020_v2.csv"))
cpue <- read.csv(here("Data", "cpue_07092020_v2.csv"))   

# Re-create issue with session_ID (when making figures)
org <- left_join(captures, cpue, by = "session_ID") %>% 
  filter(is.na(org)) # 114 obs w/ "org" as NA
chk.sID <- sort(unique(org$session_ID)) # 12 unique session_IDs

# Look for these session_ID in cpue
cpue_chk <- cpue %>% 
  filter(session_ID %in% chk.sID) 
# none found -- do not exist in cpue

## Data was filtered by ASSP when error was first encountered
ASSP <- group_by(.data = captures) %>%
  filter(species == "ASSP") %>%
  filter(band_no != "notbanded") %>%
  ungroup()
ASSPorg <- left_join(ASSP, cpue, by = "session_ID") %>%
  filter(is.na(org)) # 112 obs w/ "org" as NA (indicates session_ID does not match up in cpue sheet)
chk.sID2 <- sort(unique(ASSPorg$session_ID)) # 11 unique session_IDs

# Look for these session_ID in cpue
cpue_chk <- cpue %>%
  filter(session_ID %in% chk.sID2)
# none found -- do not exist in cpue

# What is the 12th session_ID that did not show up when ASSP was filtered?
# "20050707_SCI_ND"

## Look at session_IDs in captures that were flagged
captures_chk <- captures %>%
  filter(session_ID %in% chk.sID) 
# do not seem problematic at first look
# same session_IDs as identified in July 2020 (+1 new session)
# looks like discrepancy is b/w capture time date and other dates

# Export out
write.csv(captures_chk, here("Data", "captures_chk.csv"))

# was the issue in the CPUE sheet?


# Are there session_IDs in captures that don't exist in cpue?
c.sID <- right_join(captures, cpue, by = "session_ID") %>% 
  filter(is.na(catch_ID)) # filter session_IDs that are not associated with catch_ID
# There are 15 session_IDs that are not associated with a catch_ID
chk.sID3 <- sort(unique(c.sID$session_ID))

## Look at these session_IDs in cpue that were flagged
cpue_chk <- cpue %>%
  filter(session_ID %in% chk.sID3)
# these sessions had 0 captures, which explains why the session_ID does not show up in captures sheet

## Captures
# Compare new session ID with old session ID
captures2 <- captures %>%
  # replace.value(c("site_code"), from = "ND", to = "SR") %>% # fix ND value in site_code
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_date2 = paste(session_year, session_month, session_day, sep= "")) %>%
  # replace.value(c("site_code"), from = "SR", to = "SR1") %>% # revert to old site code
  # replace.value(c("site_code"), from = "PI", to = "PI1") %>% # revert to old site code
  mutate(session_ID2 = paste(session_date2,island_code, site_code, sep = "_"))

sessionsID.1 <- captures2 %>%
  dplyr::select(session_ID, session_ID2) %>%
  mutate(C = as.numeric(session_ID > session_ID2))

issue.1 <- sessionsID.1 %>%
  filter(C == 1)
# no difference




## CPUE
# Compare new session ID with old session ID
cpue2 <- cpue %>%
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_date2 = paste(session_year, session_month, session_day, sep= "")) %>%
  # replace.value(c("site_code"), from = "SR", to = "SR1") %>% # revert to old site code
  # replace.value(c("site_code"), from = "PI", to = "PI1") %>% # revert to old site code
  mutate(session_ID2 = paste(session_date2,island_code, site_code, sep = "_"))

sessionsID.2 <- cpue2 %>%
  dplyr::select(session_ID, session_ID2) %>%
  mutate(C = as.numeric(session_ID > session_ID2))

issue.2 <- sessionsID.2 %>%
  filter(C == 1)
# no difference

## Compare session_IDs in captures and CPUE sheet
# check that all session_IDs in CPUE are in captures
sID.r <- captures.v2 %>%
  right_join(cpue.v2, by = "session_ID") %>% 
  filter(is.na(catch_ID)) # filter session_IDs that are not associated with catch_ID

# look for these session_IDs in cpue to confirm 0 captures
chk <- sID.r$session_ID
no.caps <- cpue.v2 %>%
  filter(session_ID %in% chk) # confirm 0 captures

# check that all session_IDs in captures are in CPUE
sID.l <- captures.v2 %>%
  left_join(cpue.v2, by = "session_ID") %>% 
  filter(is.na(org)) # look for NAs in org (should all be filled in)

# NA value for CAAU capture
# catch_IDs are being assigned to non-ASSP, does it matter?
chk2 <- sID.l$session_ID
no.org <- captures.v2 %>%
  filter(session_ID %in% chk2)
no.cpue <- cpue.v2 %>%
  filter(session_ID %in% chk2) #session_ID not in cpue, why??

# look for site_code listed as "ND" in captures
unique(captures.v2$site_code)
which(captures.v2$site_code == "ND")
c320 <- captures.v2[320,]

# change site_code to SR in code  for new captures sheet

# Read in updated data

captures.new <- read.csv(here("Data", "captures.csv"))
cpue.new <- read.csv(here("Data", "cpue.csv")) 

band <- captures.new %>%
  filter(band_no == "4501-41795")


### Update database ###
# Create new captures sheet
captures.v2 <- captures %>%
  replace.value(c("site_code"), from = "ND", to = "SR") %>% # fix ND value in site_code
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_ID = paste(session_date,island_code, site_code, sep = "_")) %>%
  group_by(session_ID) %>% 
  mutate(seq = 1:n(),
         catch_ID = if_else(seq<10, paste(session_ID, seq, sep = "_0"), paste(session_ID, seq, sep = "_"))) %>% 
  ungroup() %>% 
  dplyr::select(catch_ID, session_ID, session_date, session_year, session_month, session_day, 
                island_code:notes)

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
  dplyr::select(session_ID:series_ID,net_open_1, net_close_5, net_mesh:flagged_notes)






# Export out
write.csv(captures.v2, here("Output", "captures.csv"))
write.csv(cpue.v2, here("Output", "cpue.csv"))
write.csv(sessions, here("Output", "sessions.csv"))
