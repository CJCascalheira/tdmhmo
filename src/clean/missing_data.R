# Dependencies
library(tidyverse)

# Import
redcap_raw <- read_csv("data/raw/redcap_raw.csv")

# BASIC COUNT DATA FOR PARTICIPANT SECTION --------------------------------

# Total responses
nrow(redcap_raw)

# Not completed - number
redcap_raw %>%
  filter(complete == 0) %>%
  nrow()

# Not completed - remove
redcap_raw_1 <- redcap_raw %>%
  filter(complete != 0)

# Not USA - number
redcap_raw_1 %>%
  filter(live_usa == 0) %>%
  nrow()

# Not USA - remove
redcap_raw_2 <- redcap_raw_1 %>%
  filter(live_usa != 0)

# Consent - number
redcap_raw_2 %>%
  filter(consent == 0) %>%
  nrow()

# Consent - remove
redcap_raw_3 <- redcap_raw_2 %>%
  filter(consent != 0)

# Age - number
redcap_raw_3 %>%
  filter(age < 18) %>%
  nrow()

# Age - remove
redcap_raw_4 <- redcap_raw_3 %>%
  filter(age >= 18)

# Total removed
nrow(redcap_raw) - nrow(redcap_raw_4)

# Check 1 - number
redcap_raw_4 %>%
  filter(check_1 != 1) %>%
  nrow()

# Check 1 - remove
redcap_raw_5 <- redcap_raw_4 %>%
  filter(check_1 ==1)

# Check 2 - number
redcap_raw_5 %>%
  filter(check_2 != 1)

# Check 2 - remove
redcap_raw_6 <- redcap_raw_5 %>%
  filter(check_2 == 1)

# DROP COLUMNS AND EXPORT -------------------------------------------------

# Drop unnecessary columns
redcap_raw_7 <- redcap_raw_6 %>%
  select(-c(redcap_survey_identifier, inside_out_the_transgender_mental_health_survey_timestamp,
            consent, live_usa, complete, check_1, check_2))

# Export
write_csv(redcap_raw_7, path = "data/raw/redcap_complete.csv")
