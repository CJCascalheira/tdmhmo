# Dependencies
library(tidyverse)

# Import
redcap <- read_csv("data/raw/redcap_complete.csv")

# Demographic data
redcap_demo <- redcap %>%
  select(record_id:sex_last_year)

# Scale data
redcap_scales <- redcap %>%
  select(-c(age:sex_last_year))

# RACE AND ETHNICITY ------------------------------------------------------


# GENDER ------------------------------------------------------------------

# Gender selection variables
redcap_gender <- redcap_demo %>%
  select(record_id, starts_with("gender__"))

# Gender options for trans community
gender_names <- c("nothing", "agender", "androgynous", "butch", "changes a lot", "dyke", "female", "femme", "FTM", "gender fluid", "gender neutral", "genderless", "genderqueer", "intergender", "male", "man", "MTF", "pangender", "queer", "trans guy", "transfeminine", "transgender", "transman", "transmasculine", "two spirit", "unsure", "woman", "other", "trans girl", "transwoman", "non-binary", "trans woman", "trans man")

# Gender coding
for (i in 2:length(redcap_gender)) {
  
  # Conditional and assignment
  redcap_gender[i] <- if_else(redcap_gender[i] == 1, gender_names[i], "none")
  
}
redcap_gender

# Condense to one column
gender_col <- redcap_gender %>%
  gather(key = "gender__", value = "gender", -record_id) %>%
  filter(gender != "none") %>%
  spread(key = "gender__", value = "gender") %>%
  # Unite all the columns
  unite(gender_p, gender___1:gender___9, sep = "_", remove = TRUE, na.rm = TRUE)
gender_col

# Add named gender variable to demo data frame
redcap_demo %>%
  select(-starts_with("gender__")) %>%
  left_join(gender_col, by = "record_id") %>%
  select(record_id:education, gender_p, saab, everything()) %>%
  # Assigned sex at birth
  mutate(saab = if_else(saab == 0, "female", "male"))
