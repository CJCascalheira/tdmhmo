# Dependencies
library(tidyverse)

# Import
redcap_raw <- read_csv("data/raw/redcap_raw.csv")

# Double entries based on emails being entered twice
# Calculated from an R script not included in publicly available code
double_entries <- read_csv("data/results/double_entries.csv")

# Same person given by Record IDs
# - Person 1 = 150, 905
# - Person 2 = 162, 644
# - Person 3 = 223, 799

# Therefore, keep only the first entry

# Just the record ids (to be removed) as a vector
doubles <- double_entries %>%
  filter(record_id %in% c(905, 644, 799)) %>%
  pull()
doubles

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

# Number of double entries to be reoved
redcap_raw_7 %>%
  filter(record_id %in% doubles) %>%
  nrow()

# Filter out the double entries
redcap_raw_8 <- redcap_raw_7 %>%
  filter(!(record_id %in% doubles))

# Export
write_csv(redcap_raw_8, path = "data/raw/redcap_complete.csv")
