library(here)
library(tidyverse)
captures <- readRDS(here("Working", "captures.RDS")) %>%
  filter(recapture != "SNR" &
           band_no != "unbanded") %>%
  arrange(session_date)

inds <- unique(captures$band_no)

enc <- captures %>%
  group_by(band_no) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# ind 1401-56230
ind1 <- captures %>%
  filter(band_no == "1401-56230")

# ind 1971-02327
ind2 <- captures %>%
  filter(band_no == "1971-02327")

# ind 1971-02454
ind3 <- captures %>%
  filter(band_no == "1971-02454")

# ind 1401-56333
ind4 <- captures %>%
  filter(band_no == "1401-56333")

# ind 1401-57488
ind5 <- captures %>%
  filter(band_no == "1401-57488")

# ind 1401-60851
ind6 <- captures %>%
  filter(band_no == "1401-60851")
