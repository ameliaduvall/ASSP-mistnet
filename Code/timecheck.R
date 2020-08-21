## ASSP Mistnet Time Check ##
## Amelia DuVall (ajduvall@uw.edu)
## 20 August 2020

library(here)
library(lubridate)
library(anchors)

captures <- read.csv(here("Data", "captures.csv"))
cpue <- read.csv(here("Data", "cpue.csv"))                   

## Captures
# Compare new session ID with old session ID
captures2 <- captures %>%
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_date2 = paste(session_year, session_month, session_day, sep= "")) %>%
  replace.value(c("site_code"), from = "SR", to = "SR1") %>% # revert to old site code
  replace.value(c("site_code"), from = "PI", to = "PI1") %>% # revert to old site code
  mutate(session_ID2 = paste(session_date2,island_code, site_code, sep = "_"),) 

sessionsID <- captures2 %>%
  dplyr::select(session_ID, session_ID2) %>%
  mutate(C = as.numeric(session_ID > session_ID2))

issue <- sessionsID %>%
  filter(C == 1)
# no difference

# Finalize new captures sheet
captures.v2 <- captures %>%
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
  
# Export out

## CPUE
cpue.v2 <- cpue %>% 
  mutate(session_month = ifelse(month > 9, paste(month), paste(0, month, sep = ""))) %>%
  mutate(session_day = ifelse(day > 9, paste(day), paste(0, day, sep = ""))) %>%
  rename(session_year = year) %>%
  mutate(session_date = make_date(session_year, session_month, session_day)) %>%
  mutate(session_ID = paste(session_date,island_code, site_code, sep = "_")) %>%
  dplyr::select(session_ID:long, session_date, session_year, session_month, session_day, 
                series_ID:flagged_notes)

# Export out

## Compare session_IDs across captures and CPUE sheet
session_ID_captures <- captures.v2 %>%
  dplyr::select(session_ID) 

session_ID_cpue <- cpue.v2 %>%
  dplyr::select(session_ID)

u_captures <-  unique(session_ID_captures)
u_cpue <- cpue <- unique(session_ID_cpue)

