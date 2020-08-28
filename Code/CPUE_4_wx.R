## ASSP CPUE Summary
## Amelia DuVall
## 25 June 2020
## 27 August 2020 edited

## Summarizing ASSP CPUE data to export and join with ERA5 Wx data

library(easypackages)
libraries("here", "tidyverse", "readxl", "lubridate")

## read-in data
CPUE.raw <- read.csv(here("Working", "cpue.csv"), na.strings = c("ND", "NA"))
# 238 sessions

## fix dates
CPUE <- CPUE.raw %>%
  mutate(session_date = as_date(session_date, format = "%m/%d/%Y")) %>%
  mutate_at((c("net_open_1", "net_open_2", "net_open_3", "net_open_4", "net_open_5",
               "net_close_1", "net_close_2", "net_close_3", "net_close_4", "net_close_5")), 
            ~ as_datetime(.x, format = "%m/%d/%Y %H:%M"))

summary <- CPUE %>%
  group_by(site_code) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total))
# Most used sites: AP, ESP, PI, SR, SRO (?)

## Filter to most used sites only
df <- CPUE %>% 
  filter(site_code == "AP" | site_code== "ESP" |
           site_code == "PI" | site_code == "SR")
# 183 sessions

## Look for flagged sessions
flagged <- df %>%
  filter(flagged == "Y")
# 35 sessions flagged
# Need to revisit flagged sessions and consider inclusion

df.v1 <- df %>%
  filter(!is.na(net_open_1)) # remove sessions w/o a start time

## Filter problematic flagged sessions (no net open time, changing vocalization playback)
final <- df.v1 %>% 
  mutate(start = net_open_1) %>%
  mutate(end = if_else(!is.na(net_close_5), net_close_5, 
                    if_else(!is.na(net_close_4), net_close_4, 
                           if_else(!is.na(net_close_3), net_close_3,
                                  if_else(!is.na(net_close_2), net_close_2, net_close_1))))) %>%
  dplyr::select(session_ID, island_code, site_code, lat, long, session_year, session_date, start, end)
# 167 obs

## Export
write.csv(final, here("Working", "ASSP_4_wx.csv"))
saveRDS(final, here("Working", "ASSP_4_wx.RDS"))
