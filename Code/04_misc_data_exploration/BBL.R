# BBL Storm-Petrel CA Banding Records
# BBL.R
# 2 January 2019
# Amelia DuVall ajduvall@uw.edu

# Raw data file received from Danny Bystrak (dbystrak@usgs.gov). Original file name:
# "DuVall_all_Storm-p_Bs_in_CA_201903151935.csv". File does not include recaps (?).
# This script is written to cross-reference BBL banding effort records with CHIS records. 

library(tidyverse)

BBL <- read.csv("BBL.csv")

# Preliminary summary of locations w/ number of indv. banded and number of banding days. 
summary <- group_by(.data = BBL, BANDING_YEAR, B_LOCATION_DESCRIPTION) %>% 
  summarise(No_banded = n(),
            No_days = n_distinct(BANDING_DATE)) %>% 
  ungroup()

write.csv(x = summary, file = "summary_BBL.csv")

# Do not know if "BANDING_DATE" is true date or mist-netting start date.
# Issue with duplicate "B_LOCATION_DESCRIPTION"

# Created df with distinct location descriptions. 
locs <- group_by(.data = BBL, B_LOCATION_DESCRIPTION) %>%
  distinct(B_LOCATION_DESCRIPTION) %>%
  arrange(B_LOCATION_DESCRIPTION) %>%
  ungroup()

write.csv(x = locs, file = "locs_BBL.csv")

# Created new file with Channel Islands locations grouped using "locs_BBL.csv" file.
CI_locs <- read.csv("locs_grouped_BBL.csv")

# Dropped first column in CI_locs
CI_locs_sml <- select(CI_locs, B_LOCATION_DESCRIPTION, region, island)

# Dropped unnecessary columns in BBL
BBL_sml <- select(.data = BBL, X10_MIN_BLOCK, AGE_CODE, B_LOCATION_DESCRIPTION, BAND_NUM, BANDING_DATE,
                  BANDING_YEAR, COMMENTS, PERMIT_NUM, SPECIES_NAME)

# Joined two dataframes.
BBL_locs <- left_join(BBL_sml, CI_locs_sml, by = "B_LOCATION_DESCRIPTION")

# Filter out Channel Islands locs. 
CI_bands <- group_by(.data = filter(BBL_locs, region == "Channel Islands")) %>%
  ungroup()

# Export to csv
write.csv(x = CI_bands, file = "CI_bands.csv")

# Summary of Channel Islands effort
CI_summary <- group_by(.data = CI_bands, BANDING_YEAR, island) %>% 
  summarise(No_banded = n(),
            No_days = n_distinct(BANDING_DATE)) %>% 
  ungroup()

# Export to csv
write.csv(x = CI_summary, file = "CI_summary.csv")

# Plot results
all_islands <- ggplot(data = CI_summary) +
  geom_point(mapping = aes(x = BANDING_YEAR, y = No_banded, color = island)) +
  xlab("Year") + ylab("Number storm-petrels banded") +
  ggtitle("All Islands")

pdf("all_islands.pdf")
print(all_islands)
dev.off()

by_island <- ggplot(data = CI_summary) +
  geom_point(mapping = aes(x = BANDING_YEAR, y = No_banded)) +
  xlab("Year") + ylab("Number storm-petrels banded") +
  facet_wrap(~ island, nrow = 3)

pdf("by_island.pdf")
print(by_island)
dev.off()

# Summary of banding sessions
CI_effort <- group_by(.data = CI_bands, BANDING_YEAR, BANDING_DATE, island) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

# Banding sessions by island. Filter by age code for adults banded only ("1" or "0"). 
AI_effort <- group_by(.data = filter(CI_bands, island == "Anacapa Island", 
                      AGE_CODE == "1" | AGE_CODE == "0"), BANDING_YEAR, 
                      BANDING_DATE, island, B_LOCATION_DESCRIPTION, PERMIT_NUM) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

SBI_effort <- group_by(.data = filter(CI_bands, island == "Santa Barbara Island", 
                      AGE_CODE == "1" | AGE_CODE == "0"), BANDING_YEAR, 
                      BANDING_DATE, island, B_LOCATION_DESCRIPTION, PERMIT_NUM) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

SCleI_effort <- group_by(.data = filter(CI_bands, island == "San Clemente Island",
                      AGE_CODE == "1" | AGE_CODE == "0"), BANDING_YEAR, 
                      BANDING_DATE, island, B_LOCATION_DESCRIPTION, PERMIT_NUM) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

SCI_effort <- group_by(.data = filter(CI_bands, island == "Santa Cruz Island", 
                      AGE_CODE == "1" | AGE_CODE == "0"), BANDING_YEAR, 
                      BANDING_DATE, island, B_LOCATION_DESCRIPTION, PERMIT_NUM) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

SMI_effort <- group_by(.data = filter(CI_bands, island == "San Miguel Island", 
                      AGE_CODE == "1" | AGE_CODE == "0"), BANDING_YEAR, 
                      BANDING_DATE, island, B_LOCATION_DESCRIPTION, PERMIT_NUM) %>% 
  summarise(No_banded = n()) %>%
  ungroup()

# Export tables
write.csv(x = AI_effort, file = "AI_effort.csv")
write.csv(x = SBI_effort, file = "SBI_effort.csv")
write.csv(x = SCleI_effort, file = "SCleI_effort.csv")
write.csv(x = SCI_effort, file = "SCI_effort.csv")
write.csv(x = SMI_effort, file = "SMI_effort.csv")

# Find unique PERM_NUM to inquire about permit holders. 
Permits <- group_by(.data = CI_bands, BANDING_YEAR, PERMIT_NUM, island) %>%
  arrange(BANDING_YEAR) %>%
  distinct(PERMIT_NUM) %>%
  ungroup()

# Find mist-netting sessions incorrectly 
