#CHIS ASSP Mistnet Database
#Banding QA/QC
#25-27 March 2020
#Amelia DuVall

#============================ 
library(tidyverse)
library(lubridate)
#============================ 

#Read in Banding data sheet (copied/pasted from database version 3/24/2020)
#Change NDs to be read as NAs so morph data interpreted as integers
banding <- read.csv("bandingQAQC.csv", header = TRUE, na.strings = c("NA", "ND"))

#Filter out ASSP species only and banded individuals only
ASSP <- group_by(.data = banding) %>%
  filter(species == "ASSP") %>%
  filter(band_no != "notbanded") %>%
  ungroup()

#============================ 
##Exploring unique band no's

#Create summary of band no's and no of captures per unique band no
summary <-  group_by(.data = ASSP, band_no) %>% 
  summarise(no_captures = n()) %>% 
  ungroup()

#3644 results
#Is this the number of unique bands?
NROW(unique(ASSP$band_no))
#Yes

#Summarize capture rates
summarycaptures <- group_by(summary, no_captures) %>%
  summarise(count = n()) %>%
  ungroup()

#============================ 
##Exploring recapture field

#How does unique band no's does this match recapture field data?
recap <- group_by(.data = ASSP, recapture) %>%
  summarise(no_captures = n()) %>%
  ungroup()
#3607 new captures
#41 SNR
#196 recaptures

#How to proof these data?
#Unique band numbers are not necessarily new captures 

#============================ 
##Capture time

#Change data type of time stamp and pull out hour as new field
ASSP$capture_time <- mdy_hm(ASSP$capture_time, tz="US/Pacific")
ASSP$cap_hour <- hour(ASSP$capture_time)

unique(ASSP$cap_hour)
#Some problematic data, unique values at 11, 10, 12
#4 also seems like a really late morning capture time, but plausible, double-check. 

summary(ASSP$cap_hour)
#No NA's in capture time. Double-checked database, this is correct. 
#They were filtered out (destroyed/lost bands).

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = cap_hour), binwidth = 1)
#Appears that some time stamps were not entered in military time

captimesite <- arrange(ASSP, site_code, capture_time)
#Unsure how to check min/sec of capture time without preservation of data entry order

#Isolate questionable data
cap_hour_chk <- group_by(.data = ASSP, cap_hour) %>%
  filter(cap_hour %in% c(4, 10, 11, 12)) %>%
  ungroup()

write.csv(cap_hour_chk, "captimeQAQC.csv")
#Verified capture times with raw data and changed any mistakes. Tracked changes in csv.

#============================      
##Release time

#Change data type of release time stamp and pull out hour as new field
ASSP$release_time <- mdy_hm(ASSP$release_time, tz="US/Pacific")
ASSP$rel_hour <- hour(ASSP$release_time)

unique(ASSP$rel_hour)
#Problematic data: 12
#Double-check 3am and 4am also

summary(ASSP$rel_hour)
#1920 NA's
#Min and max make sense

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = rel_hour), binwidth = 1)
#Also appears to be data not entered in military time

#Isolate questionable data
rel_hour_chk <- group_by(.data = ASSP, rel_hour) %>%
  filter(rel_hour %in% c(3, 4, 12)) %>%
  ungroup()

write.csv(rel_hour_chk, "reltimeQAQC.csv")
#Verified with raw data and changed any mistakes. Tracked changes in csv.

#============================                 
##BP

unique(ASSP$BP)
#Fix lowercase values
#All in range of values included in data dictionary

#Not sure how else to QAQC?

#============================ 
##Mass (uncorrected)

unique(ASSP$uncorr_mass)
summary(ASSP$uncorr_mass)
#Large range between 26 - 157  but it's uncorrected data
#493 NAs

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = uncorr_mass), binwidth =1)
#Bimodal! Could be due to two different weighing containers used.
#Also really high outlier (157)

#Decided not to QAQC this field, but to check mass_corr values.

#============================ 
##Mass tare
unique(ASSP$mass_tare)
summary(ASSP$mass_tare)
#494 NAs
#1 more NA in mass tar than uncorr_mass. Why?

#Decided not to QAQC this field, but to check mass_corr values.

#============================ 
##Mass (corrected)

unique(ASSP$mass_corr)
summary(ASSP$mass_corr)
#494 NAs
#large range between 6 - 135
#mean 35.45
#median 35

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = as.numeric(mass_corr)), binwidth = 1)

ggplot(data = ASSP) +
  geom_point(mapping = aes(x = mass_corr, y = year, color = as.factor(year)))
#Need to double-check outliers

#Re Adams 2016 paper, Mass (g) 36.1 ? 2.8 (f) and 34.7 ? 2.1 (m)

#Isolate questionable data
mass_corr_chk <- filter(ASSP, mass_corr < 25 | mass_corr > 50)

write.csv(mass_corr_chk, "masscorrQAQC.csv")
#Verified with raw data and changed any mistakes. Tracked changes in csv.

#============================ 
##Culmen

summary(ASSP$culmen)
#Large range between 10.50 - 24.80
#Mean at 14.51
#Medium at 14.50
#Larger values seen incongruous

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = culmen))

#Re Adams 2016 paper, Bill length (mm) 14.9 ? 0.5 (f) and 14.6 ? 0.8 (m)
#Re Pyle guide, 13.1-15.2 (95% CI)

#Isolate questionable data
culmen_chk <- filter(ASSP, culmen < 13 | culmen > 16) 

write.csv(culmen_chk, "culmenQAQC.csv")
#Verified with raw data and changed any mistakes. Tracked changes in csv.
#Seems like the range I picked (13-16) was too narrow. Most were not typos.
#Assumed values w/in (12-16.9) were not typos after reviewing data. 

#============================ 
##Skull length

summary(ASSP$skull_length)
#Range between 26 - 46.90
#Mean at 37.93
#Median at 38
#Lower values seen incongruous 

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = skull_length))

#Re Adams 2016 paper, skull length (mm) 38.1 ? 1.1 (f) and 37.9 ? 0.8 (m)
#No info in Pyle guide

#Isolate questionable data
skull_chk <- filter(ASSP, skull_length < 35 | skull_length > 41) 

write.csv(skull_chk, "skullQAQC.csv")

#============================ 
##Tarsus

summary(ASSP$tarsus)
#Range between 12.60 - 235
#Mean at 23.55
#Median at 23.45

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = tarsus))

#Re Adams 2016 paper, tarsus (mm) 23.2 ? 0.9 (f) and 23.1 ? 0.8 (m)
#Re Pyle guide, 21-25 (95% CI)

#Isolate questionable data
tarsus_chk <- filter(ASSP, tarsus < 19 | tarsus > 27) 

write.csv(tarsus_chk, "tarsusQAQC.csv")

#============================ 
##Wing

summary(ASSP$wing)
#Range between 123 - 1425
#Mean at 141
#Median at 141

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = wing))

#Re Adams 2016, max flat wing (mm) 142.7 ? 2.8 (f) and 140.4 ? 3.3 (m)
#Re Pyle guide, wing chord 132-148 (95% CI)

#Isolate questionable data
wing_chk <- filter(ASSP, wing < 130 | wing > 150) 

write.csv(wing_chk, "wingQAQC.csv")

#============================ 
##Tail

summary(ASSP$tail)
#Range between 9 - 83
#Mean 72.10
#Median 78

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = tail))

#Re Pyle, 72-84 (95% CI)
             
#Isolate questionable data
tail_chk <- filter(ASSP, tail < 71 | tail > 85) 

write.csv(tail_chk, "tailQAQC.csv")
