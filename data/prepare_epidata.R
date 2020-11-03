library(tidyverse)
library(lubridate)
library(arrow)

hospitaldata <- read_csv("reference_hospitalization_all_locs.csv.zip")

hospitaldata %>%
  mutate(
    discharges = pmax(round(allbed_mean + lead(admis_mean, n = 1) - deaths_mean - lead(allbed_mean, n = 1)),0),
    admissions_mean = round(admis_mean),
    admissions_lower = round(admis_lower),
    admissions_upper = round(admis_upper),
    icu_admissions_mean = round(newICU_mean),
    icu_admissions_lower = round(newICU_lower),
    icu_admissions_upper = round(newICU_upper),
    ) %>%
  select(
    date, 
    location = location_name, 
    discharges, 
    admissions_mean,
    admissions_lower,
    admissions_upper,
    icu_admissions_mean, 
    icu_admissions_lower, 
    icu_admissions_upper) %>%
  arrange(location, date) -> hospitaldata
write_feather(hospitaldata, "ihme_discharges_admissions.feather", compression = "uncompressed")

hospitaldata %>%
  filter(location %in% c("United States of America","Alabama","Alaska",
                         "Arizona","Arkansas","California","Colorado",
                         "Connecticut","Delaware","Florida","Georgia",
                         "Hawaii","Idaho","Illinois","Indiana","Iowa",
                         "Kansas","Kentucky","Louisiana","Maine","Maryland",
                         "Massachusetts","Michigan","Minnesota","Mississippi",
                         "Missouri","Montana","Nebraska","Nevada",
                         "New Hampshire","New Jersey","New Mexico","New York",
                         "North Carolina","North Dakota","Ohio","Oklahoma",
                         "Oregon","Pennsylvania","Rhode Island",
                         "South Carolina","South Dakota","Tennessee","Texas",
                         "Utah","Vermont","Virginia","Washington",
                         "West Virginia","Wisconsin","Wyoming",
                         "District of Columbia","Puerto Rico"
  )) %>%
  write_feather("ihme_discharges_admissions_usa.feather", compression = "uncompressed")
