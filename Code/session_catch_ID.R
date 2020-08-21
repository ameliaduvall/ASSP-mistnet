#CHIS ASSP Mistnet Database
#Updating session and catch IDs
#2 April 2020
#Amelia DuVall

#============================ 
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(mosaic)
library(foreach)
library(doParallel)
#============================ 

### READ IN DATA
## CPUE DATA (AKA METADATA)
CPUE_raw <- read.csv("CPUE.csv", na.strings=c("","NA"))

## BANDING DATA (AKA CATCHES DATA)
catches_raw <- read.csv("fix.csv", na.strings=c("","NA")) 

CPUE <- CPUE_raw %>% 
  mutate(dateStr = paste(year, month, day, sep = ""),
         date99 = if_else(day<10, paste(year, "0", month, "0", day, sep=""), 
                          paste(year, "0", month, day, sep="")),
         session_ID = paste(date99,island_code, site_code, sep = "_")) %>%
  select(session_ID:flagged_notes) %>%
  filter(TRUE)

catches <- catches_raw %>% 
              mutate(dateStr = paste(year, month, day, sep = ""),
                     date99 = if_else(day<10, paste(year, "0", month, "0", day, sep=""), 
                                      paste(year, "0", month, day, sep="")),
                     session_ID = paste(date99, island_code, site_code, sep = "_")) %>%
              group_by(session_ID) %>% 
              mutate(seq = 1:n(),
                     catch_ID = if_else(seq<10, paste(session_ID, seq, sep = "_0"), paste(session_ID, seq, sep = "_"))) %>% 
              ungroup() %>% 
              select(catch_ID:notes) %>%
              mutate(capture_time = mdy_hms(capture_time)) %>%
              mutate(capture_hour = hour(capture_time),
                     capture_min = minute(capture_time)) %>%
              filter(TRUE)

catches_hm <- catches %>% mutate(capture_time = mdy_hms(capture_time))


  
mutate(capture_time_hour = hour(capture_time),
         capture_time_min = min(capture_time))




### save data to manually review
write.csv(CPUE, file = 'CPUE.csv', row.names = FALSE)

write.csv(catches, file = 'banding_new.csv', row.names = FALSE)


# catches <- catches_raw %>% 
#   filter(island != "ND") %>% 
#   mutate(date = mdy(date),
#          # standardize site names
#          Site= mosaic::derivedFactor(
#            # ANI
#            "CC" = (island=="ANI" & site=="Cathedral Cove"),
#            "EAI_N" = (island=="ANI" & site=="EAI North side, dock area" | island=="ANI" & site=="Landing Cove Overlook"),
#            "EAI_S" = (island=="ANI" & site=="EAI South Ridge" | island=="ANI" & site=="EAI South side--near water catchment"),
#            "EAI_SW" = (island=="ANI" & site=="EAI SW end"),
#            "EAI_W" = (island=="ANI" & site=="EAI west of lighthouse"),
#            "FC" = (island=="ANI" & site =="North side Frenchy's Cove, East End, Upper Bench" | island=="ANI" & site =="Frenchy's Beach" | island=="ANI" & site =="Frenchy's Cove"),
#            "GC" = (island=="ANI" & site=="GC"),
#            "RR" = (island=="ANI" & site=="Rat Rock"),
#            "RC" = (island=="ANI" & site=="Rockfall Cove"), 
#            # PI
#            "PI1" = (island=="PI" & site=="PI1"), # & site=="1" | island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
#            # SBI
#            "AP" = (island=="SBI" & site=="Arch Point" | island=="SBI" & site=="AP"),
#            "ESP" = (island=="SBI" & site=="Eseal Point" | island=="SBI" & site=="ESP" | island=="SBI" & site=="" | island=="SBI" & site=="UNK"),
#            "NTP" = (island=="SBI" & site=="Nature Trail Plot"), 
#            "NCliffs" = (island=="SBI" & site=="North Peak Cliffs" | island=="SBI" & site=="North Cliffs"), 
#            "SR" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
#            "SP" = (island=="SBI" & site=="SignalPeak" | island=="SBI" & site=="Signal Peak"), 
#            "Sutil" = (island=="SBI" & site=="Sutil Island"), 
#            "WP" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"),
#            "WCliffs" = (island=="SBI" & site=="West Cliffs"), 
#            # SCI
#            "DR" = (island=="SCI" & site=="Diablo Rock"), 
#            "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
#            "SR2" = (island=="SR" & site=="2"),
#            "SR3" = (island=="SR" & site=="3"),
#            "LSH" = (island=="SCI" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
#            "HT" = (island=="SR" & site=="SR High Terrace-East"),
#            .default = "UNK"),
#          # create unique netting night ID
#          day = substr(date, 9, 10),
#          Month = substr(date, 6, 7),
#          dateStr = paste(year, Month, day, sep = ""),
#          sessionID = paste(dateStr,island, Site, sep = "_"),
#          # combine date and time, adjust date to actual capture date
#          capture.time_old = as.POSIXct(paste(date, capture.time), format="%Y-%m-%d %H:%M"),
#          # if "capture.time" times were actually after midnight:
#          nextDay_capture.time = capture.time_old + 24*60*60, 
#          # pull out the hour of capture event, if before 12 it would be an early morning capture:
#          hour_capture = as.numeric(hour(capture.time_old)),
#          capture_time = if_else(hour_capture <= 12, nextDay_capture.time, capture.time_old),
#          # do the same for release time
#          release.time_old = as.POSIXct(paste(date, release.time), format="%Y-%m-%d %H:%M"),
#          nextDay_release.time = release.time_old + 24*60*60, 
#          hour_release = as.numeric(hour(release.time_old)),
#          release_time = if_else(hour_release <= 12, nextDay_release.time, release.time_old)) %>% 
#   group_by(sessionID) %>% 
#   mutate(seq = 1:n(),
#          catchID = paste(sessionID, seq, sep = "_")) %>% 
#   ungroup() %>% 
#   mutate(sessionID = as.factor(sessionID),
#          Site = as.factor(Site)) %>% 
#   select(catchID, sessionID, day, Month, year, island, Site, measurer.s., capture_time, release_time, sunset:Notes) %>% #-capture.time, -capture.time_old, -nextDay_capture.time, -hour_capture, -release.time, -release.time_old, -nextDay_release.time, -hour_release) %>% 
#   filter(TRUE)

