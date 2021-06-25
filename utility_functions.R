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

exp_model_func <- function(t, K = 0.44068065575293935, lambd = 0.02564293675387818, t0 = 7, scale=1.0) {
  prob <- ifelse(t<t0, 0, scale*K * (1.0 - exp(-lambd*(t-t0)*scale)))
  return(prob)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

read_epi_can_fordates <- function(start_date,
                                  end_date,
                                  region = "CA",
                                  prop_hospitalized = 0.025,
                                  prop_direct_icu = 0.04,
                                  prop_stepped_up_icu = 0.12,
                                  delay_step_up = 5) {
  #browser()
  if (region == "USA") {
    region_code <- NULL
  } else {
    region_code <- paste0("/", region)
  }
  # REMEMBER TO PUT covidactnow_api_key.txt IN .gitignore OF PUBLIC REPO
  api_key <- readr::read_file("covidactnow_api_key.txt")
  data_location_string <- paste0("https://data.covidactnow.org/latest/us/states",
                                 region_code,
                                 ".OBSERVED_INTERVENTION.timeseries.json?apiKey=",
                                 api_key)
  data <- jsonlite::fromJSON(data_location_string)$timeseries

  if (region == "USA") {
    bind_rows(data) %>%
      group_by(date) %>%
      summarise(across(.fns = sum)) -> data
  }

  if (prop_hospitalized == "calculate") {
    data %>%
      mutate(
        prop_hospitalized = hospitalBedsRequired / currentInfected,
        deaths = lead(cumulativeDeaths, 1) - cumulativeDeaths,
        recoveries = currentInfected + lead(cumulativeInfected, 1) -
          cumulativeInfected - lead(currentInfected, 1) - deaths,
        date = lubridate::as_date(date)
      ) -> data
    #prop_hospitalized <- median(data$prop_hospitalized[data$prop_hospitalized != 0], na.rm = TRUE)
    # print("Empirical proportion hospitalized:")
    # print(prop_hospitalized)
    data %>%
      mutate(
        discharges = round(prop_hospitalized*recoveries),
        admissions = lead(hospitalBedsRequired, 1) - hospitalBedsRequired +
          discharges + deaths,
        icu_admissions = round(admissions*prop_direct_icu) +
          ifelse(!is.na(lag(admissions, delay_step_up)), round(prop_stepped_up_icu*lag(admissions, delay_step_up)), 0) #BASED ON ALTON'S DATA 07/04/20
      ) %>%
      filter(date >= start_date,
             date <= end_date) %>%
      mutate(t = seq(from = 0, to = nrow(.)-1, by = 1)) %>%
      select(date, t, recoveries, discharges, admissions, icu_admissions) %>%
      filter(!is.na(recoveries), !is.na(discharges), !is.na(admissions), !is.na(icu_admissions)) %>%
      return()
  } else {
    data %>%
      mutate(
        prop_hospitalized = hospitalBedsRequired / currentInfected,
        deaths = lead(cumulativeDeaths, 1) - cumulativeDeaths,
        recoveries = currentInfected + lead(cumulativeInfected, 1) -
          cumulativeInfected - lead(currentInfected, 1) - deaths,
        date = lubridate::as_date(date)
      ) -> data
    #prop_hospitalized <- median(data$prop_hospitalized[data$prop_hospitalized != 0], na.rm = TRUE)
    # print("Provided proportion hospitalized:")
    # print(prop_hospitalized)
    data %>%
      mutate(
        discharges = round(prop_hospitalized*recoveries),
        admissions = lead(hospitalBedsRequired, 1) - hospitalBedsRequired +
          discharges + deaths,
        icu_admissions = round(admissions*prop_direct_icu) +
          ifelse(!is.na(lag(admissions, delay_step_up)), round(prop_stepped_up_icu*lag(admissions, delay_step_up)), 0) #BASED ON ALTON'S DATA 07/04/20
      ) %>%
      filter(date >= start_date,
             date <= end_date) %>%
      mutate(t = seq(from = 0, to = nrow(.)-1, by = 1)) %>%
      select(date, t, recoveries, discharges, admissions, icu_admissions) %>%
      filter(!is.na(recoveries), !is.na(discharges), !is.na(admissions), !is.na(icu_admissions)) %>%
      return()
  }
}

lookup_region_name <- function(code) {
  lookup_table <- matrix(c("USA","AL","AK","AZ","AR","CA","CO","CT","DE","FL",
                           "GA","HI","ID","IL","IN","IA","KY","LA","ME","MD",
                           "MA","MI","MN","MS","MO","MT","NE","NV","KS","NH",
                           "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                           "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI",
                           "WY","DC","PR",
                           "United States of America","Alabama","Alaska",
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
                           ), ncol = 2)
  return(lookup_table[lookup_table[,1]==code][2])
}

read_epi_maxdate <- function(region = "CA",
                              pathprefix = "./data") {
  #browser()
  region_name <- lookup_region_name(region)
  filepath <- paste0(pathprefix, "/", "ihme_discharges_admissions_usa.feather")
  df <- arrow::read_feather(filepath) %>%
    filter(
      location == region_name
    )
  return(max(df$date))
}


read_epi_ihme_fordates <- function(start_date,
                                   end_date,
                                   region = "CA",
                                   pathprefix = "./data") {
  #browser()
  region_name <- lookup_region_name(region)
  filepath <- paste0(pathprefix, "/", "ihme_discharges_admissions_usa.feather")
  arrow::read_feather(filepath) %>%
    filter(
      location == region_name,
      date >= start_date,
      date <= end_date
      ) %>%
    mutate(t = seq(from = 0, to = nrow(.)-1, by = 1)) %>%
    return()
}

select_columns_ihme <- function(data,
                                discharges = NULL,
                                admissions,
                                icu_admissions) {
  if (admissions == "mean") {
    data %>%
      rename(admissions = admissions_mean) -> data
  } else if (admissions == "lower") {
    data %>%
      rename(admissions = admissions_lower) -> data
  } else if (admissions == "upper") {
    data %>%
      rename(admissions = admissions_upper) -> data
  }

  if (icu_admissions == "mean") {
    data %>%
      rename(icu_admissions = icu_admissions_mean) -> data
  } else if (icu_admissions == "lower") {
    data %>%
      rename(icu_admissions = icu_admissions_lower) -> data
  } else if (icu_admissions == "upper") {
    data %>%
      rename(icu_admissions = icu_admissions_upper) -> data
  }

  data %>%
    select(date, t, discharges, admissions, icu_admissions) %>%
    arrange(date) %>%
    return()
}

ma <- function(x,
               n = 7,
               sides = 1) {
  return(stats::filter(x, rep(1 / n, n), sides = sides))
}
