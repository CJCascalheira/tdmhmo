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

# DROP COLUMNS AND EXPORT -------------------------------------------------

# Drop unnecessary columns
redcap_raw_5 <- redcap_raw_4 %>%
  select(-c(redcap_survey_identifier, inside_out_the_transgender_mental_health_survey_timestamp,
            consent, live_usa, complete))

# Export
write_csv(redcap_raw_5, path = "data/raw/redcap_complete.csv")
