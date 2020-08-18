## ASSP CPUE Summary
## Amelia DuVall
## 25 June 2020

## Summarizing ASSP CPUE data to export and join with ERA5 Wx data

library(easypackages)
libraries("here", "tidyverse", "readxl")

## read-in data
CPUE <- read_excel(here("Data", "CHIS_ASSP_mistnet_database_04292020.xlsx"), 
                   sheet = "CPUE", col_names = TRUE, na = c("NA", "ND"))

summary <- CPUE %>%
  group_by(site_code) %>%
  summarise(Total=n())
# Most used sites: AP, ESP, PI1, SR1

# Filter to most used sites only
df <- CPUE %>% 
  filter(site_code == c("AP", "ESP", "PI1", "SR1")) 
## error with this filter

df2 <- CPUE %>% 
  filter(site_code == "AP" | site_code== "ESP" |
           site_code == "PI1" | site_code == "SR1")

# Look for flagged sessions
flagged <- df %>%
  filter(flagged == "Y")
which(df$flagged == "Y")

# Filter problematic flagged sessions (no net open time, changing vocalization playback)
df.v1 <- df %>%
  slice(-c(3,4,5,6,7,22)) %>%
  mutate(start = net_open_1) %>%
  mutate(end = net_close_1)

# Fix sessions with multiple net open/close
which(!is.na(df.v1$net_open_2))
# 4 cases: 5, 16, 18, 41

# Replace with true end value
df.v1$end[5] <- df.v1$net_close_5[5]
df.v1$end[16] <- df.v1$net_close_2[16] 
df.v1$end[18] <- df.v1$net_close_2[18] 
df.v1$end[41] <- df.v1$net_close_2[41] 

# Remove unnecessary columns
df.f <- df.v1 %>%
  dplyr::select(session_ID, island_code, site_code, lat, long, month, day, year,
         start, end)

# Export
write.csv(df.f, here("Data", "ASSPwx.csv"))
