# Copyright 2020-2021 Vitalant and W. Alton Russell
# Authors: Eduard Grebe, W. Alton Russell, Brian Custer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this software. If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(arrow)

# Change as applicable
setwd("~/dev/ccp_model/data/")

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
