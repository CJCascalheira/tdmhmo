# Dependencies
library(tidyverse)
library(RBtest)

# Import
redcap <- read_csv("data/raw/redcap_cleaned.csv")

# Just the instruments
redcap_scales <- redcap %>%
  select(record_id, isos_1:mhi_2) %>%
  # Recode for reverse score
  mutate(
    tis_1 = recode(tis_1, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_2 = recode(tis_2, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_6 = recode(tis_6, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_7 = recode(tis_7, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_12 = recode(tis_12, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_19 = recode(tis_19, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_25 = recode(tis_25, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_26 = recode(tis_26, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    mhi_1 = recode(mhi_1, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    mhi_5 = recode(mhi_5, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
  )

# MISSING DATA? -----------------------------------------------------------

# How many values are missing?
redcap_scales %>%
  gather(key = "scale", value = "score", -record_id) %>%
  filter(is.na(score)) %>%
  nrow()

# Are data missing completely at random (MCAR)?
redcap_scales %>%
  select(-record_id) %>%
  RBtest()

# Replace missing values
for (i in 2:length(redcap_scales)) {
  
  redcap_scales[i] <- ifelse(is.na(redcap_scales[i][[1]]), 
         mean(redcap_scales[i][[1]], na.rm = TRUE), 
         redcap_scales[i][[1]])
}

# Missing values remaining?
redcap_scales %>%
  gather(key = "scale", value = "score", -record_id) %>%
  filter(is.na(score)) %>%
  nrow()

# EXPORT SCALES -----------------------------------------------------------

# Write to file
