# ASSP Recapture Rates at CHIS
# ASSP_recap.R
# 26 November 2019
# Updated 3 January 2020, 5 January 2020
# Amelia DuVall ajduvall@uw.edu

# This script is used to calculate recapture rates (including same night recaptures) for 
# Ashy Storm-Petrels (ASSP) during mist-netting sessions at Channel Islands National
# Park from 1994-2018.

library(tidyverse)
library(lubridate)

# Read in mistnet data file
mistnet_raw <- read.csv(file = "mistnet.csv", header = TRUE)

# Rename columns
mistnet <- rename(.data = mistnet_raw, island_code = island, measurers = measurer.s., cap_time = capture.time, 
                  rel_time = release.time, band_no = bandno, recap = recapture., mass_corr = mass.cor, 
                  skl_lngth = skl.lnth, notes = Notes)

# Remove unnecessary columns
mistnet_v2 <- select(.data = mistnet, date, island_code, year, site, cap_time, species, band_no,
                      recap, notes)

# Read in CPUE data file
CPUE_raw <- read.csv(file = "CPUE.csv", header = TRUE, stringsAsFactors = FALSE)

# Rename columns
CPUE <- rename(.data = CPUE_raw, island_code = island, net_mesh = Net_mesh, net_dim = Net_dim, 
               audio = Audio_file, dB = dB_level, speaker = Speaker_system, net_open = net.open, 
               net_close = net.close, net_area = netarea, birds_min_area = birds.min.area)

# Remove unneccessary columns
CPUE_v2  <- select(.data = CPUE, date, year, island_code, site, net_mesh, net_dim, audio, dB, 
                   speaker, net_open, net_close, notes) 

# Remove unneccessary rows
CPUE_v3 <- slice(.data = CPUE_v2, 1:311)

# Add new column "island" to CPUE_v3 df
island <- c("Anacapa Island", "Santa Barbara Island", "Santa Cruz Island", "Santa Cruz Island", 
                  "San Miguel Island")
island_code <- c("ANI", "SBI", "SCI", "SR", "PI")
island_name_code <- data.frame(island, island_code)
CPUE_v4 <- left_join(CPUE_v3, island_name_code, by = "island_code")

# Create unique mistnetting session ID in CPUE_v4 df
CPUE_v5 <- unite(data = CPUE_v4, col = loc_date, island, date, sep = "_", remove = FALSE) 
CPUE_v6 <- arrange(.data = CPUE_v5, loc_date)
CPUE_v7 <- distinct(.data = CPUE_v6, loc_date)
CPUE_v8 <- add_column(.data = CPUE_v7, session_ID = c(1:length(CPUE_v7$loc_date)))
CPUE_v9 <- left_join(CPUE_v6, CPUE_v8, by = "loc_date")
CPUE_v10 <- select(.data = CPUE_v9, session_ID, year, date, island, site, net_mesh, net_dim, audio, 
                   dB, speaker, net_open, net_close, notes)

# Calculate duration for each mistnetting session
CPUE_v11 <- add_column(.data = CPUE_v10, net_open_t = hm(as.character(CPUE_v10$net_open)))







# Preliminary summary of CPUE data
CPUE_summary <- group_by(.data = CPUE_sml, year, island) %>% 
  summarise(No_sessions = n()) %>% 
  ungroup()

write.csv(x = CPUE_summary, file = "summary_CPUE.csv")

# Plot sessions over time
all_sessions <- ggplot(data = CPUE_summary) +
  geom_point(mapping = aes(x = year, y = No_sessions, color = island)) +
  xlab("Year") + ylab("Number sessions") +
  ggtitle("All Mistnetting Sessions")

pdf("all_sessions.pdf")
print(all_sessions)
dev.off()

sessions_by_island <- ggplot(data = CPUE_summary) +
  geom_point(mapping = aes(x = year, y = No_sessions)) +
  xlab("Year") + ylab("Number sessions") +
  facet_wrap(~ island, nrow = 2)

pdf("sessions_by_island.pdf")
print(sessions_by_island)
dev.off()


# Add new column "island" to mistnet df
mistnet_sml <- left_join(mistnet_sml_1, island_name_code, by = "island_code")

# Filter out other species (LESP, BLSP) and same night recaptures (SNR)
ASSP <- filter(.data = mistnet_sml, species == "ASSP", recap == "N" | recap == "Y")

# Summarize no. captured by mistnetting session date (ASSP only)
No_captured <- group_by(.data = ASSP, year, date, island) %>%
  summarise(No_captured = n()) %>% 
  ungroup()






# Filter out other species
summary(mistnet$species)
ASSPna <- mistnet[mistnet$species == "ASSP",]

# Check observations in global environment. Decrease to 3883.
# Recheck species.
summary(ASSPna$species)

# Remove "NA" rows from species column.
ASSP <- na.omit(object = ASSPna, cols = "species")

# Check observations in global environment. Decrease to 3864.
summary(ASSP$species)

# Summarize recapture column
summary(ASSP$recapture)


# Double-check unknown values.
ASSP[ASSP$recapture == "UNK",]
# Individuals were not banded. 

# Create dataframe of number of individuals captured each year. 
capturedperyear <- data.frame(summary(ASSP$year))
colnames(capturedperyear) <- "no.captured"
colnames(capturedperyear)
print(capturedperyear)

# Create dataframe for each year
yr2018 <- data.frame(ASSP[ASSP$year == 2018,])
yr2017 <- data.frame(ASSP[ASSP$year == 2017,])
yr2016 <- data.frame(ASSP[ASSP$year == 2016,])
yr2015 <- data.frame(ASSP[ASSP$year == 2015,])
yr2014 <- data.frame(ASSP[ASSP$year == 2014,])
yr2012 <- data.frame(ASSP[ASSP$year == 2012,])
yr2011 <- data.frame(ASSP[ASSP$year == 2011,])
yr2010 <- data.frame(ASSP[ASSP$year == 2010,])
yr2009 <- data.frame(ASSP[ASSP$year == 2009,])
yr2007 <- data.frame(ASSP[ASSP$year == 2007,])
yr2006 <- data.frame(ASSP[ASSP$year == 2006,])
yr2005 <- data.frame(ASSP[ASSP$year == 2005,])
yr2004 <- data.frame(ASSP[ASSP$year == 2004,])
yr1999 <- data.frame(ASSP[ASSP$year == 1999,])
yr1996 <- data.frame(ASSP[ASSP$year == 1996,])
yr1995 <- data.frame(ASSP[ASSP$year == 1995,])
yr1994 <- data.frame(ASSP[ASSP$year == 1994,])

# Try as a for loop
# years <- c(1994:2018)
# nyears <- length(years)

#for i in 1:nyears) {
#  yr[i] <- data.frame(ASSP[ASSP$year == [i],])
#}

# Summarize recaptures for each year.
recap2018 <- data.frame(summary(yr2018$recapture))
colnames(recap2018) <- "2018"
recap2017 <- data.frame(summary(yr2017$recapture))
colnames(recap2017) <- "2017"
recap2016 <- data.frame(summary(yr2016$recapture))
colnames(recap2016) <- "2016"
recap2015 <- data.frame(summary(yr2015$recapture))
colnames(recap2015) <- "2015"
recap2014 <- data.frame(summary(yr2014$recapture))
colnames(recap2014) <- "2014"
recap2012 <- data.frame(summary(yr2012$recapture))
colnames(recap2012) <- "2012"
recap2011 <- data.frame(summary(yr2011$recapture))
colnames(recap2011) <- "2011"
recap2010 <- data.frame(summary(yr2010$recapture))
colnames(recap2010) <- "2010"
recap2009 <- data.frame(summary(yr2009$recapture))
colnames(recap2009) <- "2009"
recap2007 <- data.frame(summary(yr2007$recapture))
colnames(recap2007) <- "2007"
recap2006 <- data.frame(summary(yr2006$recapture))
colnames(recap2006) <- "2006"
recap2005 <- data.frame(summary(yr2005$recapture))
colnames(recap2005) <- "2005"
recap2004 <- data.frame(summary(yr2004$recapture))
colnames(recap2004) <- "2004"
recap1999 <- data.frame(summary(yr1999$recapture))
colnames(recap1999) <- "1999"
recap1996 <- data.frame(summary(yr1996$recapture))
colnames(recap1996) <- "1996"
recap1995 <- data.frame(summary(yr1995$recapture))
colnames(recap1995) <- "1995"
recap1994 <- data.frame(summary(yr1994$recapture))
colnames(recap1994) <- "1994"

summaryrecap <- data.frame(cbind(recap2018, recap2017, recap2016, recap2015, 
                 recap2014, recap2012, recap2011, recap2010, recap2009,
                 recap2007, recap2006, recap2005, recap2004, recap1999, 
                 recap1996, recap1995, recap1994))

# Delete first row
summaryrecap <- summaryrecap[-1,]

# Rename rows
rownames(summaryrecap)

rownames(summaryrecap) <- c("new", "same-night", "unk", "recap")
head(summaryrecap)

yearsum <- apply(X = summaryrecap, MARGIN = 2, FUN = sum)

summaryrecap <- rbind(summaryrecap, yearsum)

rownames(summaryrecap) <- c("new", "same-night", "unk", "recap", "sum")
head(summaryrecap)

write.csv(x = summaryrecap, file = "ASSPsummaryrecap.csv")   

