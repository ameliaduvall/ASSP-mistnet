## ASSP Mistnet Time Check ##
## Amelia DuVall (ajduvall@uw.edu)
## 20 August 2020

library(here)
library(lubridate)
library(anchors)
library(tidyverse)

# Read-in data from earlier version
captures.new <- read.csv(here("Data", "captures.csv"))
cpue.new <- read.csv(here("Data", "cpue.csv")) 

band <- captures.new %>%
  filter(band_no == "4501-41795")

## Captures
# Compare new session ID with old session ID
captures2 <- captures %>%
  replace.value(c("site_code"), from = "ND", to = "SR") %>% # fix ND value in site_code
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_date2 = paste(session_year, session_month, session_day, sep= "")) %>%
  replace.value(c("site_code"), from = "SR", to = "SR1") %>% # revert to old site code
  replace.value(c("site_code"), from = "PI", to = "PI1") %>% # revert to old site code
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
  replace.value(c("site_code"), from = "SR", to = "SR1") %>% # revert to old site code
  replace.value(c("site_code"), from = "PI", to = "PI1") %>% # revert to old site code
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
