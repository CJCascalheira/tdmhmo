# Dependencies
library(tidyverse)

# Import
redcap_raw <- read_csv("full_data_practice/data/raw/redcap_raw.csv")

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
nrow(redcap_raw_1)

# Not USA - number
redcap_raw_1 %>%
  filter(live_usa == 0) %>%
  nrow()

# Not USA - remove
redcap_raw_2 <- redcap_raw_1 %>%
  filter(live_usa != 0)
nrow(redcap_raw_2)

# Consent - number
redcap_raw_2 %>%
  filter(consent == 0) %>%
  nrow()

# Consent - remove
redcap_raw_3 <- redcap_raw_2 %>%
  filter(consent != 0)
nrow(redcap_raw_3)

# Age - number
redcap_raw_3 %>%
  filter(age < 18 | is.na(age)) %>%
  nrow()

# Age - remove
redcap_raw_4 <- redcap_raw_3 %>%
  filter(age >= 18)
nrow(redcap_raw_4)

# Check 1 - number
redcap_raw_4 %>%
  filter(check_1 != 1 | is.na(check_1)) %>%
  nrow()

# Check 1 - remove
redcap_raw_5 <- redcap_raw_4 %>%
  filter(check_1 == 1 )
nrow(redcap_raw_5)

# Check 2 - number
redcap_raw_5 %>%
  filter(check_2 != 1 | is.na(check_2)) %>%
  nrow()

# Check 2 - remove
redcap_raw_6 <- redcap_raw_5 %>%
  filter(check_2 == 1)
nrow(redcap_raw_6)

# Total removed
nrow(redcap_raw) - nrow(redcap_raw_6)

# DROP COLUMNS AND EXPORT -------------------------------------------------

# Drop unnecessary columns
redcap_raw_7 <- redcap_raw_6 %>%
  select(-c(redcap_survey_identifier, inside_out_the_transgender_mental_health_survey_timestamp,
            consent, live_usa, complete, check_1, check_2))

# Export
write_csv(redcap_raw_7, path = "full_data_practice/data/raw/redcap_complete.csv")
